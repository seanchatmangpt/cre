/**
 * Setup Wizard Component
 * Multi-step onboarding wizard UI with validation and progress tracking
 */

import React, { useState, useCallback, useEffect } from 'react';
import { WizardStep, WizardState, FormField, SetupConfiguration, FieldOption } from './types';

interface SetupWizardProps {
  steps: WizardStep[];
  onComplete: (formData: Record<string, unknown>) => Promise<void>;
  onStepChange?: (stepIndex: number) => void;
  onError?: (error: Error) => void;
}

interface FormErrors {
  [key: string]: string;
}

const SetupWizardComponent: React.FC<SetupWizardProps> = ({
  steps,
  onComplete,
  onStepChange,
  onError,
}) => {
  const [state, setState] = useState<WizardState>({
    currentStepIndex: 0,
    formData: {},
    errors: new Map(),
    completionPercentage: 0,
    startTime: new Date(),
  });

  const [isSubmitting, setIsSubmitting] = useState(false);
  const [showValidationErrors, setShowValidationErrors] = useState(false);

  /**
   * Validate a single form field
   */
  const validateField = useCallback(
    (field: FormField, value: unknown): string | null => {
      // Check required
      if (field.required && !value) {
        return `${field.label} is required`;
      }

      // Check validation rules
      if (field.validation) {
        const { pattern, minLength, maxLength, custom } = field.validation;

        if (pattern && typeof value === 'string' && !pattern.test(value)) {
          return `${field.label} format is invalid`;
        }

        if (minLength && typeof value === 'string' && value.length < minLength) {
          return `${field.label} must be at least ${minLength} characters`;
        }

        if (maxLength && typeof value === 'string' && value.length > maxLength) {
          return `${field.label} must be at most ${maxLength} characters`;
        }

        if (custom && !custom(value)) {
          return `${field.label} validation failed`;
        }
      }

      return null;
    },
    []
  );

  /**
   * Validate current step fields
   */
  const validateCurrentStep = useCallback((): boolean => {
    const currentStep = steps[state.currentStepIndex];
    const errors: FormErrors = {};

    currentStep.fields.forEach((field) => {
      const value = state.formData[field.name];
      const error = validateField(field, value);
      if (error) {
        errors[field.name] = error;
      }
    });

    setState((prev) => ({
      ...prev,
      errors: new Map(Object.entries(errors)),
    }));

    return Object.keys(errors).length === 0;
  }, [steps, state, validateField]);

  /**
   * Handle field value changes
   */
  const handleFieldChange = useCallback(
    (fieldName: string, value: unknown) => {
      setState((prev) => ({
        ...prev,
        formData: {
          ...prev.formData,
          [fieldName]: value,
        },
      }));

      // Clear error for this field if it becomes valid
      const currentStep = steps[state.currentStepIndex];
      const field = currentStep.fields.find((f) => f.name === fieldName);
      if (field) {
        const error = validateField(field, value);
        setState((prev) => {
          const newErrors = new Map(prev.errors);
          if (error) {
            newErrors.set(fieldName, error);
          } else {
            newErrors.delete(fieldName);
          }
          return { ...prev, errors: newErrors };
        });
      }
    },
    [steps, state.currentStepIndex, validateField]
  );

  /**
   * Move to next step
   */
  const handleNextStep = useCallback((): void => {
    if (!validateCurrentStep()) {
      setShowValidationErrors(true);
      return;
    }

    setShowValidationErrors(false);

    const nextIndex = state.currentStepIndex + 1;
    if (nextIndex < steps.length) {
      const newCompletion = ((nextIndex + 1) / steps.length) * 100;
      setState((prev) => ({
        ...prev,
        currentStepIndex: nextIndex,
        completionPercentage: newCompletion,
      }));

      onStepChange?.(nextIndex);
    }
  }, [state.currentStepIndex, steps.length, validateCurrentStep, onStepChange]);

  /**
   * Move to previous step
   */
  const handlePreviousStep = useCallback((): void => {
    const prevIndex = state.currentStepIndex - 1;
    if (prevIndex >= 0) {
      const newCompletion = ((prevIndex + 1) / steps.length) * 100;
      setState((prev) => ({
        ...prev,
        currentStepIndex: prevIndex,
        completionPercentage: newCompletion,
      }));

      onStepChange?.(prevIndex);
    }
  }, [state.currentStepIndex, steps.length, onStepChange]);

  /**
   * Handle wizard completion
   */
  const handleComplete = useCallback(async (): Promise<void> => {
    if (!validateCurrentStep()) {
      setShowValidationErrors(true);
      return;
    }

    setIsSubmitting(true);
    try {
      await onComplete(state.formData);
      setState((prev) => ({
        ...prev,
        completionPercentage: 100,
      }));
    } catch (error) {
      const err = error instanceof Error ? error : new Error('Unknown error');
      onError?.(err);
    } finally {
      setIsSubmitting(false);
    }
  }, [state.formData, validateCurrentStep, onComplete, onError]);

  const currentStep = steps[state.currentStepIndex];
  const isLastStep = state.currentStepIndex === steps.length - 1;
  const isFirstStep = state.currentStepIndex === 0;

  return (
    <div className="setup-wizard">
      {/* Progress Bar */}
      <div className="wizard-progress">
        <div className="progress-bar-container">
          <div className="progress-bar-fill" style={{ width: `${state.completionPercentage}%` }} />
        </div>
        <p className="progress-text">
          Step {state.currentStepIndex + 1} of {steps.length}
          {' - '}
          {Math.round(state.completionPercentage)}%
        </p>
      </div>

      {/* Step Indicator */}
      <div className="wizard-steps">
        {steps.map((step, index) => (
          <div
            key={step.id}
            className={`step-indicator ${
              index <= state.currentStepIndex ? 'completed' : ''
            } ${index === state.currentStepIndex ? 'active' : ''}`}
          >
            <div className="step-number">{index + 1}</div>
            <div className="step-title">{step.title}</div>
          </div>
        ))}
      </div>

      {/* Current Step */}
      <div className="wizard-step-content">
        <h2>{currentStep.title}</h2>
        {currentStep.description && <p className="step-description">{currentStep.description}</p>}
        {currentStep.helpText && <div className="help-text">{currentStep.helpText}</div>}

        {/* Form Fields */}
        <form className="wizard-form" onSubmit={(e) => e.preventDefault()}>
          {currentStep.fields.map((field) => (
            <div key={field.name} className="form-field">
              <label htmlFor={field.name}>
                {field.label}
                {field.required && <span className="required">*</span>}
              </label>

              {/* Text/Email/Number Input */}
              {['text', 'email', 'number'].includes(field.type) && (
                <input
                  id={field.name}
                  type={field.type}
                  placeholder={field.placeholder}
                  value={(state.formData[field.name] as string) || ''}
                  onChange={(e) => handleFieldChange(field.name, e.target.value)}
                  disabled={isSubmitting}
                />
              )}

              {/* Textarea */}
              {field.type === 'textarea' && (
                <textarea
                  id={field.name}
                  placeholder={field.placeholder}
                  value={(state.formData[field.name] as string) || ''}
                  onChange={(e) => handleFieldChange(field.name, e.target.value)}
                  disabled={isSubmitting}
                />
              )}

              {/* Select */}
              {field.type === 'select' && (
                <select
                  id={field.name}
                  value={(state.formData[field.name] as string) || ''}
                  onChange={(e) => handleFieldChange(field.name, e.target.value)}
                  disabled={isSubmitting}
                >
                  <option value="">Select an option...</option>
                  {field.options?.map((option: FieldOption) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </select>
              )}

              {/* Checkbox */}
              {field.type === 'checkbox' && (
                <input
                  id={field.name}
                  type="checkbox"
                  checked={(state.formData[field.name] as boolean) || false}
                  onChange={(e) => handleFieldChange(field.name, e.target.checked)}
                  disabled={isSubmitting}
                />
              )}

              {/* Error Message */}
              {showValidationErrors && state.errors.has(field.name) && (
                <div className="error-message">{state.errors.get(field.name)}</div>
              )}

              {/* Help Text */}
              {field.helpText && <small className="field-help">{field.helpText}</small>}
            </div>
          ))}
        </form>

        {/* Estimated Time */}
        <div className="step-footer">
          <small>Estimated time: {currentStep.estimatedMinutes} minute(s)</small>
        </div>
      </div>

      {/* Navigation Buttons */}
      <div className="wizard-navigation">
        <button
          onClick={handlePreviousStep}
          disabled={isFirstStep || isSubmitting}
          className="btn btn-secondary"
        >
          Previous
        </button>

        {isLastStep ? (
          <button
            onClick={handleComplete}
            disabled={isSubmitting}
            className="btn btn-primary"
          >
            {isSubmitting ? 'Completing...' : 'Complete Setup'}
          </button>
        ) : (
          <button
            onClick={handleNextStep}
            disabled={isSubmitting}
            className="btn btn-primary"
          >
            Next
          </button>
        )}
      </div>

      {/* Validation Summary */}
      {showValidationErrors && state.errors.size > 0 && (
        <div className="validation-summary">
          <p>Please fix the following errors before proceeding:</p>
          <ul>
            {Array.from(state.errors.values()).map((error, index) => (
              <li key={index}>{error}</li>
            ))}
          </ul>
        </div>
      )}

      <style>{`
        .setup-wizard {
          width: 100%;
          max-width: 800px;
          margin: 0 auto;
          padding: 20px;
          background: #f9f9f9;
          border-radius: 8px;
        }

        .wizard-progress {
          margin-bottom: 30px;
        }

        .progress-bar-container {
          width: 100%;
          height: 8px;
          background: #e0e0e0;
          border-radius: 4px;
          overflow: hidden;
          margin-bottom: 10px;
        }

        .progress-bar-fill {
          height: 100%;
          background: linear-gradient(90deg, #007bff, #0056b3);
          transition: width 0.3s ease;
        }

        .progress-text {
          font-size: 14px;
          color: #666;
          text-align: center;
        }

        .wizard-steps {
          display: flex;
          justify-content: space-between;
          margin-bottom: 40px;
          gap: 10px;
        }

        .step-indicator {
          flex: 1;
          text-align: center;
          display: flex;
          flex-direction: column;
          align-items: center;
          gap: 8px;
        }

        .step-number {
          width: 40px;
          height: 40px;
          border-radius: 50%;
          background: #e0e0e0;
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: bold;
          color: #666;
          transition: all 0.3s ease;
        }

        .step-indicator.completed .step-number {
          background: #28a745;
          color: white;
        }

        .step-indicator.active .step-number {
          background: #007bff;
          color: white;
          box-shadow: 0 0 0 3px rgba(0, 123, 255, 0.25);
        }

        .step-title {
          font-size: 12px;
          color: #666;
          max-width: 80px;
          word-wrap: break-word;
        }

        .step-indicator.active .step-title {
          font-weight: bold;
          color: #007bff;
        }

        .wizard-step-content {
          background: white;
          padding: 30px;
          border-radius: 8px;
          margin-bottom: 30px;
        }

        .wizard-step-content h2 {
          margin: 0 0 10px 0;
          color: #333;
        }

        .step-description {
          color: #666;
          margin: 0 0 20px 0;
        }

        .help-text {
          background: #e7f3ff;
          border-left: 4px solid #007bff;
          padding: 12px;
          margin: 15px 0;
          border-radius: 4px;
          color: #004085;
        }

        .wizard-form {
          display: flex;
          flex-direction: column;
          gap: 20px;
        }

        .form-field {
          display: flex;
          flex-direction: column;
          gap: 6px;
        }

        .form-field label {
          font-weight: 500;
          color: #333;
          font-size: 14px;
        }

        .required {
          color: #dc3545;
          margin-left: 4px;
        }

        .form-field input,
        .form-field textarea,
        .form-field select {
          padding: 10px;
          border: 1px solid #ddd;
          border-radius: 4px;
          font-size: 14px;
          transition: border-color 0.3s ease;
        }

        .form-field input:focus,
        .form-field textarea:focus,
        .form-field select:focus {
          outline: none;
          border-color: #007bff;
          box-shadow: 0 0 0 3px rgba(0, 123, 255, 0.1);
        }

        .form-field input:disabled,
        .form-field textarea:disabled,
        .form-field select:disabled {
          background: #f5f5f5;
          color: #999;
          cursor: not-allowed;
        }

        .error-message {
          color: #dc3545;
          font-size: 13px;
          margin-top: 4px;
        }

        .field-help {
          color: #999;
          font-size: 12px;
        }

        .step-footer {
          padding-top: 20px;
          border-top: 1px solid #eee;
          text-align: right;
        }

        .wizard-navigation {
          display: flex;
          gap: 10px;
          justify-content: space-between;
        }

        .btn {
          padding: 12px 30px;
          border: none;
          border-radius: 4px;
          font-size: 14px;
          font-weight: 500;
          cursor: pointer;
          transition: all 0.3s ease;
        }

        .btn-primary {
          background: #007bff;
          color: white;
        }

        .btn-primary:hover:not(:disabled) {
          background: #0056b3;
        }

        .btn-secondary {
          background: #6c757d;
          color: white;
        }

        .btn-secondary:hover:not(:disabled) {
          background: #545b62;
        }

        .btn:disabled {
          opacity: 0.5;
          cursor: not-allowed;
        }

        .validation-summary {
          background: #f8d7da;
          border: 1px solid #f5c6cb;
          color: #721c24;
          padding: 15px;
          border-radius: 4px;
          margin-top: 20px;
        }

        .validation-summary ul {
          margin: 10px 0 0 0;
          padding-left: 20px;
        }

        .validation-summary li {
          margin: 5px 0;
        }
      `}</style>
    </div>
  );
};

export default SetupWizardComponent;
