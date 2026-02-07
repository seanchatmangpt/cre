/**
 * Onboarding System Type Definitions
 * Covers all onboarding workflow, configuration, and metrics
 */

export interface User {
  id: string;
  email: string;
  name: string;
  organizationId: string;
  role: 'admin' | 'user' | 'manager';
  createdAt: Date;
}

export interface OnboardingState {
  userId: string;
  completedSteps: string[];
  currentStep: OnboardingStep;
  configuration: SetupConfiguration;
  startedAt: Date;
  completedAt?: Date;
  abandonedAt?: Date;
}

export enum OnboardingStep {
  WELCOME = 'welcome',
  BASIC_SETUP = 'basic_setup',
  PROFILE_CONFIGURATION = 'profile_configuration',
  INTEGRATION_SETUP = 'integration_setup',
  TRAINING_MATERIALS = 'training_materials',
  COMPLETION = 'completion',
}

export interface SetupConfiguration {
  organizationName?: string;
  organizationSize?: number;
  department?: string;
  primaryUseCase?: string;
  integrationType?: 'api' | 'webhook' | 'none';
  customSettings?: Record<string, unknown>;
  timezone?: string;
  language?: string;
}

export interface WelcomeEmailTemplate {
  id: string;
  name: string;
  subject: string;
  htmlContent: string;
  textContent: string;
  variables: string[];
  createdAt: Date;
  updatedAt: Date;
}

export interface EmailSchedule {
  step: OnboardingStep;
  delayMinutes: number;
  templateId: string;
  enabled: boolean;
}

export interface TrainingMaterial {
  id: string;
  title: string;
  category: 'video' | 'article' | 'tutorial' | 'documentation';
  contentUrl: string;
  estimatedMinutes: number;
  difficulty: 'beginner' | 'intermediate' | 'advanced';
  tags: string[];
  completed: boolean;
  completedAt?: Date;
}

export interface TrainingCurriculum {
  userId: string;
  materials: TrainingMaterial[];
  completedCount: number;
  totalMinutes: number;
  nextRecommendedMaterial?: TrainingMaterial;
  completionPercentage: number;
}

export interface SuccessMetric {
  id: string;
  metricName: string;
  value: number | string;
  unit: string;
  category: 'engagement' | 'adoption' | 'feature_usage' | 'completion';
  timestamp: Date;
  userId?: string;
  organizationId?: string;
}

export interface OnboardingMetrics {
  userId: string;
  organizationId: string;
  startDate: Date;
  completionDate?: Date;
  timeToComplete?: number; // in minutes
  emailOpenRate: number; // percentage
  stepCompletionRates: Map<OnboardingStep, number>;
  featuresUsed: string[];
  trainingCompletionRate: number;
  overallHealthScore: number; // 0-100
  recommendations: string[];
}

export interface WizardStep {
  id: string;
  title: string;
  description: string;
  fields: FormField[];
  optional: boolean;
  helpText?: string;
  estimatedMinutes: number;
}

export interface FormField {
  name: string;
  label: string;
  type: 'text' | 'email' | 'number' | 'select' | 'checkbox' | 'textarea';
  required: boolean;
  placeholder?: string;
  options?: FieldOption[];
  validation?: {
    pattern?: RegExp;
    minLength?: number;
    maxLength?: number;
    custom?: (value: unknown) => boolean;
  };
  helpText?: string;
}

export interface FieldOption {
  value: string;
  label: string;
  description?: string;
}

export interface WizardState {
  currentStepIndex: number;
  formData: Record<string, unknown>;
  errors: Map<string, string>;
  completionPercentage: number;
  startTime: Date;
}

export interface OnboardingAnalytics {
  totalUsersStarted: number;
  totalUsersCompleted: number;
  completionRate: number;
  averageTimeToCompletion: number;
  stepDropoffRates: Map<OnboardingStep, number>;
  mostCommonIssues: string[];
  emailEngagementMetrics: EmailMetrics;
  trainingMetrics: TrainingMetrics;
}

export interface EmailMetrics {
  totalSent: number;
  openRate: number;
  clickRate: number;
  unsubscribeRate: number;
  bounceRate: number;
  byTemplate: Map<string, EmailMetricDetail>;
}

export interface EmailMetricDetail {
  sent: number;
  opened: number;
  clicked: number;
  bounced: number;
}

export interface TrainingMetrics {
  startedCount: number;
  completedCount: number;
  completionRate: number;
  averageTimePerMaterial: number;
  mostAccessedMaterials: string[];
  difficulty_distribution: {
    beginner: number;
    intermediate: number;
    advanced: number;
  };
}

export interface OnboardingError extends Error {
  code: string;
  statusCode: number;
  details?: Record<string, unknown>;
}
