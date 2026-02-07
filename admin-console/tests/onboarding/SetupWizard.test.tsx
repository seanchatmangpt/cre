/**
 * Setup Wizard Component Tests
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import SetupWizardComponent from '@/onboarding/SetupWizardComponent';
import { WizardStep, FormField } from '@/onboarding/types';

describe('SetupWizardComponent', () => {
  const mockSteps: WizardStep[] = [
    {
      id: 'step-1',
      title: 'Basic Information',
      description: 'Enter your basic information',
      fields: [
        {
          name: 'name',
          label: 'Full Name',
          type: 'text',
          required: true,
          placeholder: 'John Doe',
        },
        {
          name: 'email',
          label: 'Email',
          type: 'email',
          required: true,
          placeholder: 'john@example.com',
        },
      ],
      optional: false,
      estimatedMinutes: 5,
    },
    {
      id: 'step-2',
      title: 'Organization',
      description: 'Tell us about your organization',
      fields: [
        {
          name: 'orgName',
          label: 'Organization Name',
          type: 'text',
          required: true,
        },
        {
          name: 'size',
          label: 'Organization Size',
          type: 'select',
          required: true,
          options: [
            { value: 'small', label: '1-50 employees' },
            { value: 'medium', label: '51-250 employees' },
            { value: 'large', label: '250+ employees' },
          ],
        },
      ],
      optional: false,
      estimatedMinutes: 5,
    },
  ];

  const mockOnComplete = vi.fn();

  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('rendering', () => {
    it('should render the wizard', () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      expect(screen.getByText('Basic Information')).toBeInTheDocument();
    });

    it('should display progress bar', () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      expect(screen.getByText(/Step 1 of 2/)).toBeInTheDocument();
    });

    it('should show all step indicators', () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      expect(screen.getByText('Basic Information')).toBeInTheDocument();
      expect(screen.getByText('Organization')).toBeInTheDocument();
    });

    it('should display form fields', () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      expect(screen.getByLabelText('Full Name')).toBeInTheDocument();
      expect(screen.getByLabelText('Email')).toBeInTheDocument();
    });

    it('should show required field indicators', () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const labels = screen.getAllByText(/\*/);
      expect(labels.length).toBeGreaterThan(0);
    });
  });

  describe('navigation', () => {
    it('should disable previous button on first step', () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const previousButton = screen.getByText('Previous');
      expect(previousButton).toBeDisabled();
    });

    it('should navigate to next step', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const nameInput = screen.getByPlaceholderText('John Doe');
      const emailInput = screen.getByPlaceholderText('john@example.com');
      const nextButton = screen.getByText('Next');

      await userEvent.type(nameInput, 'John Doe');
      await userEvent.type(emailInput, 'john@example.com');
      await userEvent.click(nextButton);

      await waitFor(() => {
        expect(screen.getByText('Organization')).toBeInTheDocument();
      });
    });

    it('should navigate back to previous step', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const nameInput = screen.getByPlaceholderText('John Doe');
      const emailInput = screen.getByPlaceholderText('john@example.com');
      const nextButton = screen.getByText('Next');

      await userEvent.type(nameInput, 'John Doe');
      await userEvent.type(emailInput, 'john@example.com');
      await userEvent.click(nextButton);

      const previousButton = await screen.findByText('Previous');
      await userEvent.click(previousButton);

      await waitFor(() => {
        expect(screen.getByPlaceholderText('John Doe')).toBeInTheDocument();
      });
    });

    it('should update progress on navigation', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      expect(screen.getByText(/50%/)).toBeInTheDocument();

      const nameInput = screen.getByPlaceholderText('John Doe');
      const emailInput = screen.getByPlaceholderText('john@example.com');
      const nextButton = screen.getByText('Next');

      await userEvent.type(nameInput, 'John Doe');
      await userEvent.type(emailInput, 'john@example.com');
      await userEvent.click(nextButton);

      await waitFor(() => {
        expect(screen.getByText(/100%/)).toBeInTheDocument();
      });
    });
  });

  describe('validation', () => {
    it('should prevent navigation with empty required fields', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const nextButton = screen.getByText('Next');
      await userEvent.click(nextButton);

      await waitFor(() => {
        expect(screen.getByText(/required/i)).toBeInTheDocument();
      });
    });

    it('should validate email format', async () => {
      const emailSteps: WizardStep[] = [
        {
          id: 'email-step',
          title: 'Email',
          fields: [
            {
              name: 'email',
              label: 'Email',
              type: 'email',
              required: true,
              validation: {
                pattern: /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
              },
            },
          ],
          optional: false,
          estimatedMinutes: 5,
        },
      ];

      render(<SetupWizardComponent steps={emailSteps} onComplete={mockOnComplete} />);

      const emailInput = screen.getByLabelText('Email');
      const completeButton = screen.getByText('Complete Setup');

      await userEvent.type(emailInput, 'invalid-email');
      await userEvent.click(completeButton);

      await waitFor(() => {
        expect(screen.getByText(/invalid/i)).toBeInTheDocument();
      });
    });

    it('should show validation errors', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const nextButton = screen.getByText('Next');
      await userEvent.click(nextButton);

      await waitFor(() => {
        expect(screen.getByText(/validation summary/i)).toBeInTheDocument();
      });
    });

    it('should clear errors when field is corrected', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const nextButton = screen.getByText('Next');
      await userEvent.click(nextButton);

      const nameInput = screen.getByPlaceholderText('John Doe');
      await userEvent.type(nameInput, 'John Doe');

      await waitFor(() => {
        // Error for this field should be gone
        const errors = screen.queryAllByText(/full name is required/i);
        expect(errors.length).toBe(0);
      });
    });
  });

  describe('form interactions', () => {
    it('should handle text input changes', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const nameInput = screen.getByPlaceholderText('John Doe') as HTMLInputElement;
      await userEvent.type(nameInput, 'Jane Smith');

      expect(nameInput.value).toBe('Jane Smith');
    });

    it('should handle select input changes', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      const nameInput = screen.getByPlaceholderText('John Doe');
      const emailInput = screen.getByPlaceholderText('john@example.com');
      const nextButton = screen.getByText('Next');

      await userEvent.type(nameInput, 'John Doe');
      await userEvent.type(emailInput, 'john@example.com');
      await userEvent.click(nextButton);

      const sizeSelect = await screen.findByLabelText('Organization Size');
      await userEvent.selectOptions(sizeSelect, 'large');

      expect((sizeSelect as HTMLSelectElement).value).toBe('large');
    });

    it('should handle checkbox input changes', async () => {
      const checkboxSteps: WizardStep[] = [
        {
          id: 'checkbox-step',
          title: 'Preferences',
          fields: [
            {
              name: 'newsletter',
              label: 'Subscribe to newsletter',
              type: 'checkbox',
              required: false,
            },
          ],
          optional: true,
          estimatedMinutes: 2,
        },
      ];

      render(<SetupWizardComponent steps={checkboxSteps} onComplete={mockOnComplete} />);

      const checkbox = screen.getByLabelText('Subscribe to newsletter');
      await userEvent.click(checkbox);

      expect((checkbox as HTMLInputElement).checked).toBe(true);
    });
  });

  describe('completion', () => {
    it('should call onComplete when finishing wizard', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      // Fill first step
      const nameInput = screen.getByPlaceholderText('John Doe');
      const emailInput = screen.getByPlaceholderText('john@example.com');
      await userEvent.type(nameInput, 'John Doe');
      await userEvent.type(emailInput, 'john@example.com');

      let nextButton = screen.getByText('Next');
      await userEvent.click(nextButton);

      // Fill second step
      const orgNameInput = screen.getByDisplayValue('');
      const sizeSelect = screen.getByLabelText('Organization Size');

      await userEvent.type(orgNameInput, 'Test Corp');
      await userEvent.selectOptions(sizeSelect, 'large');

      const completeButton = await screen.findByText('Complete Setup');
      await userEvent.click(completeButton);

      await waitFor(() => {
        expect(mockOnComplete).toHaveBeenCalled();
      });
    });

    it('should pass form data to onComplete', async () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      // Fill steps
      const nameInput = screen.getByPlaceholderText('John Doe');
      const emailInput = screen.getByPlaceholderText('john@example.com');
      await userEvent.type(nameInput, 'John Doe');
      await userEvent.type(emailInput, 'john@example.com');

      const nextButton = screen.getByText('Next');
      await userEvent.click(nextButton);

      const completeButton = await screen.findByText('Complete Setup');
      await userEvent.click(completeButton);

      await waitFor(() => {
        expect(mockOnComplete).toHaveBeenCalledWith(
          expect.objectContaining({
            name: 'John Doe',
            email: 'john@example.com',
          })
        );
      });
    });
  });

  describe('estimated time', () => {
    it('should display estimated time for current step', () => {
      render(<SetupWizardComponent steps={mockSteps} onComplete={mockOnComplete} />);

      expect(screen.getByText(/Estimated time: 5 minute/)).toBeInTheDocument();
    });
  });
});
