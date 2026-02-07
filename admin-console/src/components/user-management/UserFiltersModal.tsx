import React, { useState } from 'react';
import { Modal } from '../common/Modal';
import { Button } from '../common/Button';
import { Select } from '../common/Select';
import { UserRole, UserStatus } from '../../types/user';

export interface UserFiltersModalProps {
  isOpen: boolean;
  onClose: () => void;
  onApplyFilters?: (filters: Record<string, string>) => void;
}

export const UserFiltersModal: React.FC<UserFiltersModalProps> = ({
  isOpen,
  onClose,
  onApplyFilters,
}) => {
  const [filters, setFilters] = useState({
    role: '',
    status: '',
  });

  const handleApply = () => {
    onApplyFilters?.(filters);
    onClose();
  };

  const handleReset = () => {
    setFilters({ role: '', status: '' });
  };

  const roleOptions = [
    { label: 'All Roles', value: '' },
    ...Object.values(UserRole).map((role) => ({
      label: role.charAt(0).toUpperCase() + role.slice(1),
      value: role,
    })),
  ];

  const statusOptions = [
    { label: 'All Statuses', value: '' },
    ...Object.values(UserStatus).map((status) => ({
      label: status.charAt(0).toUpperCase() + status.slice(1),
      value: status,
    })),
  ];

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title="Filter Users"
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
          label="Role"
          value={filters.role}
          onChange={(e) => setFilters({ ...filters, role: e.target.value })}
          options={roleOptions}
        />

        <Select
          label="Status"
          value={filters.status}
          onChange={(e) => setFilters({ ...filters, status: e.target.value })}
          options={statusOptions}
        />
      </div>
    </Modal>
  );
};
