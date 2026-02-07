import React, { useState } from 'react';
import { Save, RotateCcw } from 'lucide-react';
import { Button } from '../common/Button';
import { ConfigCategory, ConfigGroup } from '../../types/config';
import { ConfigSection } from './ConfigSection';

export const Configuration: React.FC = () => {
  const [hasChanges, setHasChanges] = useState(false);
  const [selectedCategory, setSelectedCategory] = useState<ConfigCategory>(ConfigCategory.GENERAL);
  const [isLoading, setIsLoading] = useState(false);

  // Mock config data
  const configGroups: ConfigGroup[] = [
    {
      category: ConfigCategory.GENERAL,
      items: [
        {
          key: 'app.name',
          label: 'Application Name',
          description: 'The name of your application',
          type: 'string' as const,
          category: ConfigCategory.GENERAL,
          value: 'Admin Console',
          defaultValue: 'Admin Console',
          required: true,
        },
        {
          key: 'app.timezone',
          label: 'Timezone',
          type: 'select' as const,
          category: ConfigCategory.GENERAL,
          value: 'UTC',
          defaultValue: 'UTC',
          required: true,
          options: [
            { label: 'UTC', value: 'UTC' },
            { label: 'America/New_York', value: 'America/New_York' },
            { label: 'Europe/London', value: 'Europe/London' },
          ],
        },
      ],
    },
    {
      category: ConfigCategory.SECURITY,
      items: [
        {
          key: 'security.mfa_required',
          label: 'Require MFA',
          description: 'Require multi-factor authentication for all users',
          type: 'boolean' as const,
          category: ConfigCategory.SECURITY,
          value: true,
          defaultValue: false,
          required: false,
        },
        {
          key: 'security.session_timeout',
          label: 'Session Timeout',
          description: 'Session timeout in minutes',
          type: 'number' as const,
          category: ConfigCategory.SECURITY,
          value: 30,
          defaultValue: 30,
          required: true,
          validation: {
            min: 5,
            max: 1440,
            errorMessage: 'Must be between 5 and 1440 minutes',
          },
        },
      ],
    },
  ];

  const categories = Object.values(ConfigCategory);

  const handleSave = async () => {
    setIsLoading(true);
    try {
      await new Promise((resolve) => setTimeout(resolve, 1000));
      console.log('Saving configuration...');
      setHasChanges(false);
    } catch (error) {
      console.error('Error saving configuration:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const handleReset = () => {
    if (confirm('Are you sure you want to reset all changes?')) {
      setHasChanges(false);
      console.log('Resetting configuration...');
    }
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h1 className="text-2xl font-bold text-gray-900">Configuration</h1>
        <div className="flex gap-2">
          {hasChanges && (
            <Button
              variant="ghost"
              leftIcon={<RotateCcw className="w-4 h-4" />}
              onClick={handleReset}
            >
              Reset
            </Button>
          )}
          <Button
            leftIcon={<Save className="w-4 h-4" />}
            onClick={handleSave}
            isLoading={isLoading}
            disabled={!hasChanges}
          >
            Save Changes
          </Button>
        </div>
      </div>

      <div className="flex gap-6">
        <div className="w-64 bg-white rounded-lg shadow p-4">
          <nav className="space-y-1">
            {categories.map((category) => (
              <button
                key={category}
                onClick={() => setSelectedCategory(category)}
                className={`w-full text-left px-4 py-2 rounded-md transition-colors ${
                  selectedCategory === category
                    ? 'bg-blue-50 text-blue-700 font-medium'
                    : 'text-gray-700 hover:bg-gray-50'
                }`}
              >
                {category.charAt(0).toUpperCase() + category.slice(1)}
              </button>
            ))}
          </nav>
        </div>

        <div className="flex-1 space-y-6">
          {configGroups
            .filter((group) => group.category === selectedCategory)
            .map((group) => (
              <ConfigSection
                key={group.category}
                group={group}
                onChange={() => setHasChanges(true)}
              />
            ))}
        </div>
      </div>
    </div>
  );
};
