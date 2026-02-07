import React, { useState } from 'react';
import { Input } from '../common/Input';
import { Select } from '../common/Select';
import { ConfigGroup, ConfigItem, ConfigType } from '../../types/config';

export interface ConfigSectionProps {
  group: ConfigGroup;
  onChange: () => void;
}

export const ConfigSection: React.FC<ConfigSectionProps> = ({ group, onChange }) => {
  const [values, setValues] = useState<Record<string, unknown>>(
    group.items.reduce((acc, item) => ({ ...acc, [item.key]: item.value }), {})
  );

  const handleChange = (key: string, value: unknown) => {
    setValues({ ...values, [key]: value });
    onChange();
  };

  const renderField = (item: ConfigItem) => {
    switch (item.type) {
      case ConfigType.STRING:
        return (
          <Input
            key={item.key}
            label={item.label}
            value={String(values[item.key] || '')}
            onChange={(e) => handleChange(item.key, e.target.value)}
            required={item.required}
            helperText={item.description}
          />
        );

      case ConfigType.NUMBER:
        return (
          <Input
            key={item.key}
            type="number"
            label={item.label}
            value={Number(values[item.key] || 0)}
            onChange={(e) => handleChange(item.key, Number(e.target.value))}
            required={item.required}
            helperText={item.description}
            min={item.validation?.min}
            max={item.validation?.max}
          />
        );

      case ConfigType.BOOLEAN:
        return (
          <div key={item.key} className="flex items-center justify-between">
            <div>
              <label className="text-sm font-medium text-gray-700">{item.label}</label>
              {item.description && (
                <p className="text-sm text-gray-500 mt-1">{item.description}</p>
              )}
            </div>
            <label className="relative inline-flex items-center cursor-pointer">
              <input
                type="checkbox"
                checked={Boolean(values[item.key])}
                onChange={(e) => handleChange(item.key, e.target.checked)}
                className="sr-only peer"
              />
              <div className="w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-blue-600"></div>
            </label>
          </div>
        );

      case ConfigType.SELECT:
        return (
          <Select
            key={item.key}
            label={item.label}
            value={String(values[item.key] || '')}
            onChange={(e) => handleChange(item.key, e.target.value)}
            options={item.options || []}
            required={item.required}
            helperText={item.description}
          />
        );

      case ConfigType.JSON:
        return (
          <div key={item.key} className="space-y-2">
            <label className="block text-sm font-medium text-gray-700">{item.label}</label>
            {item.description && (
              <p className="text-sm text-gray-500">{item.description}</p>
            )}
            <textarea
              value={JSON.stringify(values[item.key] || {}, null, 2)}
              onChange={(e) => {
                try {
                  handleChange(item.key, JSON.parse(e.target.value));
                } catch {
                  // Invalid JSON, don't update
                }
              }}
              className="block w-full rounded-md border border-gray-300 px-3 py-2 font-mono text-sm"
              rows={6}
            />
          </div>
        );

      default:
        return null;
    }
  };

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h2 className="text-lg font-semibold text-gray-900 mb-6">
        {group.category.charAt(0).toUpperCase() + group.category.slice(1)} Settings
      </h2>
      <div className="space-y-6">{group.items.map((item) => renderField(item))}</div>
    </div>
  );
};
