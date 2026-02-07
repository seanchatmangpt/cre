import React, { useState } from 'react';
import { Modal } from '../common/Modal';
import { Button } from '../common/Button';
import { Select } from '../common/Select';
import { Input } from '../common/Input';
import { AuditAction, AuditSeverity } from '../../types/audit';

export interface AuditFiltersModalProps {
  isOpen: boolean;
  onClose: () => void;
  onApplyFilters?: (filters: Record<string, string>) => void;
}

export const AuditFiltersModal: React.FC<AuditFiltersModalProps> = ({
  isOpen,
  onClose,
  onApplyFilters,
}) => {
  const [filters, setFilters] = useState({
    action: '',
    severity: '',
    status: '',
    startDate: '',
    endDate: '',
  });

  const handleApply = () => {
    onApplyFilters?.(filters);
    onClose();
  };

  const handleReset = () => {
    setFilters({
      action: '',
      severity: '',
      status: '',
      startDate: '',
      endDate: '',
    });
  };

  const actionOptions = [
    { label: 'All Actions', value: '' },
    ...Object.values(AuditAction).map((action) => ({
      label: action.replace('_', ' ').charAt(0).toUpperCase() + action.replace('_', ' ').slice(1),
      value: action,
    })),
  ];

  const severityOptions = [
    { label: 'All Severities', value: '' },
    ...Object.values(AuditSeverity).map((severity) => ({
      label: severity.charAt(0).toUpperCase() + severity.slice(1),
      value: severity,
    })),
  ];

  const statusOptions = [
    { label: 'All Statuses', value: '' },
    { label: 'Success', value: 'success' },
    { label: 'Failure', value: 'failure' },
  ];

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title="Filter Audit Logs"
      footer={
        <div className="flex justify-between w-full">
          <Button variant="ghost" onClick={handleReset}>
            Reset
          </Button>
          <div className="flex gap-2">
            <Button variant="ghost" onClick={onClose}>
              Cancel
            </Button>
            <Button onClick={handleApply}>Apply Filters</Button>
          </div>
        </div>
      }
    >
      <div className="space-y-4">
        <Select
          label="Action"
          value={filters.action}
          onChange={(e) => setFilters({ ...filters, action: e.target.value })}
          options={actionOptions}
        />

        <Select
          label="Severity"
          value={filters.severity}
          onChange={(e) => setFilters({ ...filters, severity: e.target.value })}
          options={severityOptions}
        />

        <Select
          label="Status"
          value={filters.status}
          onChange={(e) => setFilters({ ...filters, status: e.target.value })}
          options={statusOptions}
        />

        <div className="grid grid-cols-2 gap-4">
          <Input
            label="Start Date"
            type="datetime-local"
            value={filters.startDate}
            onChange={(e) => setFilters({ ...filters, startDate: e.target.value })}
          />
          <Input
            label="End Date"
            type="datetime-local"
            value={filters.endDate}
            onChange={(e) => setFilters({ ...filters, endDate: e.target.value })}
          />
        </div>
      </div>
    </Modal>
  );
};
