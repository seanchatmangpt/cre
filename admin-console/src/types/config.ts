export enum ConfigCategory {
  GENERAL = 'general',
  SECURITY = 'security',
  NOTIFICATIONS = 'notifications',
  INTEGRATIONS = 'integrations',
  ADVANCED = 'advanced',
}

export enum ConfigType {
  STRING = 'string',
  NUMBER = 'number',
  BOOLEAN = 'boolean',
  SELECT = 'select',
  MULTISELECT = 'multiselect',
  JSON = 'json',
}

export interface ConfigOption {
  label: string;
  value: string | number;
}

export interface ConfigItem {
  key: string;
  label: string;
  description?: string;
  type: ConfigType;
  category: ConfigCategory;
  value: string | number | boolean | string[] | Record<string, unknown>;
  defaultValue: string | number | boolean | string[] | Record<string, unknown>;
  required: boolean;
  options?: ConfigOption[];
  validation?: {
    min?: number;
    max?: number;
    pattern?: string;
    errorMessage?: string;
  };
}

export interface ConfigUpdate {
  key: string;
  value: string | number | boolean | string[] | Record<string, unknown>;
}

export interface ConfigGroup {
  category: ConfigCategory;
  items: ConfigItem[];
}
