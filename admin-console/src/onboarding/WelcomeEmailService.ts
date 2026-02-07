/**
 * Welcome Email Service
 * Manages email templates, scheduling, and delivery for onboarding users
 */

import {
  WelcomeEmailTemplate,
  EmailSchedule,
  OnboardingStep,
  User,
  EmailMetrics,
  EmailMetricDetail,
} from './types';

interface EmailProvider {
  sendEmail(to: string, subject: string, html: string, text: string): Promise<string>;
  getStatus(messageId: string): Promise<{ opened: boolean; clicked: boolean; bounced: boolean }>;
}

interface ScheduledEmail {
  id: string;
  userId: string;
  templateId: string;
  scheduledFor: Date;
  sent: boolean;
  sentAt?: Date;
  messageId?: string;
  status?: { opened: boolean; clicked: boolean; bounced: boolean };
}

export class WelcomeEmailService {
  private templates: Map<string, WelcomeEmailTemplate> = new Map();
  private schedules: EmailSchedule[] = [];
  private scheduledEmails: ScheduledEmail[] = [];
  private emailMetrics: Map<string, EmailMetricDetail> = new Map();

  constructor(private emailProvider: EmailProvider) {
    this.initializeDefaultTemplates();
    this.initializeDefaultSchedules();
  }

  /**
   * Initialize default email templates
   */
  private initializeDefaultTemplates(): void {
    const templates: WelcomeEmailTemplate[] = [
      {
        id: 'welcome_email',
        name: 'Welcome Email',
        subject: 'Welcome to {{organizationName}}! Let\'s get started',
        htmlContent: `
          <h1>Welcome {{userName}}!</h1>
          <p>We're excited to have you join {{organizationName}}.</p>
          <p>Click the button below to start your onboarding journey:</p>
          <a href="{{onboardingLink}}" style="padding: 10px 20px; background-color: #007bff; color: white; text-decoration: none; border-radius: 5px;">Start Setup Wizard</a>
          <p>If you have any questions, our support team is here to help.</p>
        `,
        textContent: `
          Welcome {{userName}}!

          We're excited to have you join {{organizationName}}.

          Click the link below to start your onboarding journey:
          {{onboardingLink}}

          If you have any questions, our support team is here to help.
        `,
        variables: ['userName', 'organizationName', 'onboardingLink'],
        createdAt: new Date(),
        updatedAt: new Date(),
      },
      {
        id: 'profile_setup_reminder',
        name: 'Profile Setup Reminder',
        subject: 'Reminder: Complete your profile {{userName}}',
        htmlContent: `
          <h1>Don't forget to complete your profile!</h1>
          <p>Hi {{userName}},</p>
          <p>We noticed you haven't finished setting up your profile yet. This only takes a few minutes and helps us personalize your experience.</p>
          <a href="{{profileLink}}" style="padding: 10px 20px; background-color: #007bff; color: white; text-decoration: none; border-radius: 5px;">Complete Profile</a>
        `,
        textContent: `
          Don't forget to complete your profile!

          Hi {{userName}},

          We noticed you haven't finished setting up your profile yet. This only takes a few minutes.

          {{profileLink}}
        `,
        variables: ['userName', 'profileLink'],
        createdAt: new Date(),
        updatedAt: new Date(),
      },
      {
        id: 'training_materials_ready',
        name: 'Training Materials Ready',
        subject: 'Your personalized training materials are ready {{userName}}',
        htmlContent: `
          <h1>Your training materials are ready!</h1>
          <p>Hi {{userName}},</p>
          <p>Based on your setup, we've curated personalized training materials for you:</p>
          <ul>
            <li>{{material1}}</li>
            <li>{{material2}}</li>
            <li>{{material3}}</li>
          </ul>
          <a href="{{trainingLink}}" style="padding: 10px 20px; background-color: #28a745; color: white; text-decoration: none; border-radius: 5px;">View Training Materials</a>
        `,
        textContent: `
          Your training materials are ready!

          Hi {{userName}},

          Based on your setup, we've curated personalized training materials for you.

          {{trainingLink}}
        `,
        variables: ['userName', 'material1', 'material2', 'material3', 'trainingLink'],
        createdAt: new Date(),
        updatedAt: new Date(),
      },
      {
        id: 'onboarding_complete',
        name: 'Onboarding Complete',
        subject: 'Welcome to the team, {{userName}}!',
        htmlContent: `
          <h1>You're all set, {{userName}}!</h1>
          <p>Congratulations on completing your onboarding. You're now ready to make the most of our platform.</p>
          <p>Quick tips to get started:</p>
          <ul>
            <li>Explore the dashboard</li>
            <li>Check out the feature tour</li>
            <li>Connect with your team</li>
          </ul>
          <a href="{{dashboardLink}}" style="padding: 10px 20px; background-color: #17a2b8; color: white; text-decoration: none; border-radius: 5px;">Go to Dashboard</a>
        `,
        textContent: `
          You're all set, {{userName}}!

          Congratulations on completing your onboarding. You're now ready to make the most of our platform.

          {{dashboardLink}}
        `,
        variables: ['userName', 'dashboardLink'],
        createdAt: new Date(),
        updatedAt: new Date(),
      },
    ];

    templates.forEach((template) => {
      this.templates.set(template.id, template);
      this.emailMetrics.set(template.id, {
        sent: 0,
        opened: 0,
        clicked: 0,
        bounced: 0,
      });
    });
  }

  /**
   * Initialize default email schedule
   */
  private initializeDefaultSchedules(): void {
    this.schedules = [
      {
        step: OnboardingStep.WELCOME,
        delayMinutes: 0,
        templateId: 'welcome_email',
        enabled: true,
      },
      {
        step: OnboardingStep.PROFILE_CONFIGURATION,
        delayMinutes: 60,
        templateId: 'profile_setup_reminder',
        enabled: true,
      },
      {
        step: OnboardingStep.TRAINING_MATERIALS,
        delayMinutes: 240,
        templateId: 'training_materials_ready',
        enabled: true,
      },
      {
        step: OnboardingStep.COMPLETION,
        delayMinutes: 0,
        templateId: 'onboarding_complete',
        enabled: true,
      },
    ];
  }

  /**
   * Schedule welcome emails for a user
   */
  async scheduleWelcomeEmails(user: User, onboardingLink: string): Promise<void> {
    const enabledSchedules = this.schedules.filter((s) => s.enabled);

    for (const schedule of enabledSchedules) {
      const scheduledFor = new Date(Date.now() + schedule.delayMinutes * 60 * 1000);
      const scheduledEmail: ScheduledEmail = {
        id: `${user.id}-${schedule.step}-${Date.now()}`,
        userId: user.id,
        templateId: schedule.templateId,
        scheduledFor,
        sent: schedule.delayMinutes === 0, // Send immediately if delay is 0
      };

      this.scheduledEmails.push(scheduledEmail);

      // If immediate send, send now
      if (schedule.delayMinutes === 0) {
        await this.sendScheduledEmail(scheduledEmail, user, onboardingLink);
      }
    }
  }

  /**
   * Send a scheduled email
   */
  private async sendScheduledEmail(
    scheduledEmail: ScheduledEmail,
    user: User,
    onboardingLink: string
  ): Promise<void> {
    const template = this.templates.get(scheduledEmail.templateId);
    if (!template) {
      throw new Error(`Template ${scheduledEmail.templateId} not found`);
    }

    const variables = this.buildVariables(user, onboardingLink);
    const htmlContent = this.interpolateTemplate(template.htmlContent, variables);
    const textContent = this.interpolateTemplate(template.textContent, variables);
    const subject = this.interpolateTemplate(template.subject, variables);

    try {
      const messageId = await this.emailProvider.sendEmail(user.email, subject, htmlContent, textContent);
      scheduledEmail.messageId = messageId;
      scheduledEmail.sent = true;
      scheduledEmail.sentAt = new Date();

      // Update metrics
      const metric = this.emailMetrics.get(scheduledEmail.templateId);
      if (metric) {
        metric.sent += 1;
      }
    } catch (error) {
      throw new Error(`Failed to send email to ${user.email}: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  /**
   * Build variable object for email template
   */
  private buildVariables(user: User, onboardingLink: string): Record<string, string> {
    return {
      userName: user.name,
      organizationName: 'Your Organization', // This should come from org data
      onboardingLink,
      profileLink: `${onboardingLink}?step=profile`,
      trainingLink: `${onboardingLink}?step=training`,
      dashboardLink: `${onboardingLink}?step=dashboard`,
      material1: 'Getting Started Guide',
      material2: 'Feature Overview',
      material3: 'Best Practices',
    };
  }

  /**
   * Interpolate template with variables
   */
  private interpolateTemplate(template: string, variables: Record<string, string>): string {
    let result = template;
    Object.entries(variables).forEach(([key, value]) => {
      const regex = new RegExp(`{{${key}}}`, 'g');
      result = result.replace(regex, value);
    });
    return result;
  }

  /**
   * Process pending scheduled emails
   */
  async processPendingEmails(user: User, onboardingLink: string): Promise<void> {
    const now = new Date();
    const pendingEmails = this.scheduledEmails.filter(
      (e) => !e.sent && e.scheduledFor <= now && e.userId === user.id
    );

    for (const email of pendingEmails) {
      await this.sendScheduledEmail(email, user, onboardingLink);
    }
  }

  /**
   * Update email metrics based on tracking
   */
  async updateEmailMetrics(): Promise<void> {
    for (const email of this.scheduledEmails) {
      if (email.messageId && email.status === undefined) {
        try {
          const status = await this.emailProvider.getStatus(email.messageId);
          email.status = status;

          const metric = this.emailMetrics.get(email.templateId);
          if (metric) {
            if (status.opened) metric.opened += 1;
            if (status.clicked) metric.clicked += 1;
            if (status.bounced) metric.bounced += 1;
          }
        } catch (error) {
          console.error(`Failed to update metrics for ${email.messageId}:`, error);
        }
      }
    }
  }

  /**
   * Get email metrics
   */
  getEmailMetrics(): Map<string, EmailMetricDetail> {
    return new Map(this.emailMetrics);
  }

  /**
   * Create custom email template
   */
  createTemplate(template: WelcomeEmailTemplate): void {
    if (this.templates.has(template.id)) {
      throw new Error(`Template ${template.id} already exists`);
    }
    this.templates.set(template.id, template);
    this.emailMetrics.set(template.id, {
      sent: 0,
      opened: 0,
      clicked: 0,
      bounced: 0,
    });
  }

  /**
   * Update email schedule
   */
  updateSchedule(step: OnboardingStep, schedule: Partial<EmailSchedule>): void {
    const index = this.schedules.findIndex((s) => s.step === step);
    if (index === -1) {
      throw new Error(`Schedule for step ${step} not found`);
    }
    this.schedules[index] = { ...this.schedules[index], ...schedule };
  }

  /**
   * Get all templates
   */
  getTemplates(): WelcomeEmailTemplate[] {
    return Array.from(this.templates.values());
  }

  /**
   * Get template by ID
   */
  getTemplate(id: string): WelcomeEmailTemplate | undefined {
    return this.templates.get(id);
  }
}
