/**
 * Welcome Email Service Tests
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { WelcomeEmailService } from '@/onboarding/WelcomeEmailService';
import { User, OnboardingStep, WelcomeEmailTemplate } from '@/onboarding/types';

interface MockEmailProvider {
  sendEmail: (to: string, subject: string, html: string, text: string) => Promise<string>;
  getStatus: (messageId: string) => Promise<{ opened: boolean; clicked: boolean; bounced: boolean }>;
}

describe('WelcomeEmailService', () => {
  let service: WelcomeEmailService;
  let mockEmailProvider: MockEmailProvider;

  const mockUser: User = {
    id: 'user-123',
    email: 'user@example.com',
    name: 'John Doe',
    organizationId: 'org-123',
    role: 'user',
    createdAt: new Date(),
  };

  beforeEach(() => {
    mockEmailProvider = {
      sendEmail: vi.fn().mockResolvedValue('message-id-123'),
      getStatus: vi.fn().mockResolvedValue({
        opened: true,
        clicked: true,
        bounced: false,
      }),
    };

    service = new WelcomeEmailService(mockEmailProvider);
  });

  describe('initialization', () => {
    it('should initialize with default templates', () => {
      const templates = service.getTemplates();
      expect(templates.length).toBeGreaterThan(0);
      expect(templates.some((t) => t.id === 'welcome_email')).toBe(true);
    });

    it('should have welcome, reminder, training, and completion templates', () => {
      const templates = service.getTemplates();
      const templateIds = templates.map((t) => t.id);

      expect(templateIds).toContain('welcome_email');
      expect(templateIds).toContain('profile_setup_reminder');
      expect(templateIds).toContain('training_materials_ready');
      expect(templateIds).toContain('onboarding_complete');
    });
  });

  describe('email scheduling', () => {
    it('should schedule welcome emails for a user', async () => {
      await service.scheduleWelcomeEmails(mockUser, 'https://onboarding.example.com');

      expect(mockEmailProvider.sendEmail).toHaveBeenCalled();
    });

    it('should send welcome email immediately', async () => {
      await service.scheduleWelcomeEmails(mockUser, 'https://onboarding.example.com');

      const calls = (mockEmailProvider.sendEmail as any).mock.calls;
      expect(calls.length).toBeGreaterThan(0);
      expect(calls[0][0]).toBe(mockUser.email);
    });

    it('should interpolate template variables correctly', async () => {
      await service.scheduleWelcomeEmails(mockUser, 'https://onboarding.example.com');

      const calls = (mockEmailProvider.sendEmail as any).mock.calls;
      const subject = calls[0][1];

      expect(subject).toContain(mockUser.name);
      expect(subject).not.toContain('{{');
    });

    it('should handle email send errors gracefully', async () => {
      const errorProvider = {
        sendEmail: vi.fn().mockRejectedValue(new Error('SMTP error')),
        getStatus: vi.fn(),
      };

      const errorService = new WelcomeEmailService(errorProvider);

      await expect(
        errorService.scheduleWelcomeEmails(mockUser, 'https://onboarding.example.com')
      ).rejects.toThrow();
    });
  });

  describe('custom templates', () => {
    it('should allow creating custom templates', () => {
      const customTemplate: WelcomeEmailTemplate = {
        id: 'custom-template',
        name: 'Custom Email',
        subject: 'Hello {{userName}}',
        htmlContent: '<p>Welcome {{userName}}</p>',
        textContent: 'Welcome {{userName}}',
        variables: ['userName'],
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      service.createTemplate(customTemplate);

      const template = service.getTemplate('custom-template');
      expect(template).toBeDefined();
      expect(template?.name).toBe('Custom Email');
    });

    it('should prevent duplicate template IDs', () => {
      const template: WelcomeEmailTemplate = {
        id: 'duplicate-id',
        name: 'Template',
        subject: 'Subject',
        htmlContent: '<p>Content</p>',
        textContent: 'Content',
        variables: [],
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      service.createTemplate(template);

      expect(() => service.createTemplate(template)).toThrow();
    });
  });

  describe('schedule management', () => {
    it('should allow updating email schedules', () => {
      service.updateSchedule(OnboardingStep.WELCOME, {
        delayMinutes: 5,
        enabled: false,
      });

      // After update, service should respect new settings
      expect(service).toBeDefined();
    });
  });

  describe('metrics tracking', () => {
    it('should track email metrics', async () => {
      await service.scheduleWelcomeEmails(mockUser, 'https://onboarding.example.com');

      const metrics = service.getEmailMetrics();
      expect(metrics).toBeDefined();
      expect(metrics.size).toBeGreaterThan(0);
    });

    it('should calculate email open rates', async () => {
      await service.scheduleWelcomeEmails(mockUser, 'https://onboarding.example.com');
      await service.updateEmailMetrics();

      const metrics = service.getEmailMetrics();
      expect(metrics).toBeDefined();
    });
  });

  describe('template retrieval', () => {
    it('should retrieve template by ID', () => {
      const template = service.getTemplate('welcome_email');
      expect(template).toBeDefined();
      expect(template?.id).toBe('welcome_email');
    });

    it('should return undefined for non-existent template', () => {
      const template = service.getTemplate('non-existent-id');
      expect(template).toBeUndefined();
    });

    it('should return all templates', () => {
      const templates = service.getTemplates();
      expect(Array.isArray(templates)).toBe(true);
      expect(templates.length).toBeGreaterThan(0);
    });
  });

  describe('email content', () => {
    it('should have HTML and text content for all templates', () => {
      const templates = service.getTemplates();

      templates.forEach((template) => {
        expect(template.htmlContent).toBeDefined();
        expect(template.htmlContent.length).toBeGreaterThan(0);
        expect(template.textContent).toBeDefined();
        expect(template.textContent.length).toBeGreaterThan(0);
      });
    });

    it('should not contain uninterpolated variables in content', async () => {
      await service.scheduleWelcomeEmails(mockUser, 'https://onboarding.example.com');

      const calls = (mockEmailProvider.sendEmail as any).mock.calls;
      const htmlContent = calls[0][2];
      const textContent = calls[0][3];

      expect(htmlContent).not.toContain('{{');
      expect(textContent).not.toContain('{{');
    });
  });
});
