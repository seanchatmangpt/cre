import React, { useState, useEffect } from 'react';
import { Modal } from '../common/Modal';
import { Button } from '../common/Button';
import { Input } from '../common/Input';
import { Select } from '../common/Select';
import { User, UserRole, UserStatus, CreateUserInput, UpdateUserInput } from '../../types/user';

export interface UserModalProps {
  isOpen: boolean;
  onClose: () => void;
  user?: User | null;
}

export const UserModal: React.FC<UserModalProps> = ({ isOpen, onClose, user }) => {
  const [formData, setFormData] = useState<CreateUserInput | UpdateUserInput>({
    email: '',
    username: '',
    firstName: '',
    lastName: '',
    role: UserRole.USER,
    password: '',
  });
  const [errors, setErrors] = useState<Record<string, string>>({});
  const [isLoading, setIsLoading] = useState(false);

  useEffect(() => {
    if (user) {
      setFormData({
        email: user.email,
        username: user.username,
        firstName: user.firstName,
        lastName: user.lastName,
        role: user.role,
        status: user.status,
      });
    } else {
      setFormData({
        email: '',
        username: '',
        firstName: '',
        lastName: '',
        role: UserRole.USER,
        password: '',
      });
    }
    setErrors({});
  }, [user, isOpen]);

  const validate = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (!formData.email) {
      newErrors.email = 'Email is required';
    } else if (!/\S+@\S+\.\S+/.test(formData.email)) {
      newErrors.email = 'Invalid email format';
    }

    if (!formData.username) {
      newErrors.username = 'Username is required';
    }

    if (!formData.firstName) {
      newErrors.firstName = 'First name is required';
    }

    if (!formData.lastName) {
      newErrors.lastName = 'Last name is required';
    }

    if (!user && !(formData as CreateUserInput).password) {
      newErrors.password = 'Password is required';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!validate()) return;

    setIsLoading(true);
    try {
      // API call here
      await new Promise((resolve) => setTimeout(resolve, 1000));
      console.log('Submitting:', formData);
      onClose();
    } catch (error) {
      console.error('Error saving user:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!user || !confirm('Are you sure you want to delete this user?')) return;

    setIsLoading(true);
    try {
      // API call here
      await new Promise((resolve) => setTimeout(resolve, 1000));
      console.log('Deleting user:', user.id);
      onClose();
    } catch (error) {
      console.error('Error deleting user:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const roleOptions = Object.values(UserRole).map((role) => ({
    label: role.charAt(0).toUpperCase() + role.slice(1),
    value: role,
  }));

  const statusOptions = Object.values(UserStatus).map((status) => ({
    label: status.charAt(0).toUpperCase() + status.slice(1),
    value: status,
  }));

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title={user ? 'Edit User' : 'Create User'}
      size="lg"
      footer={
        <div className="flex justify-between w-full">
          <div>
            {user && (
              <Button variant="danger" onClick={handleDelete} isLoading={isLoading}>
                Delete User
              </Button>
            )}
          </div>
          <div className="flex gap-2">
            <Button variant="ghost" onClick={onClose}>
              Cancel
            </Button>
            <Button onClick={handleSubmit} isLoading={isLoading}>
              {user ? 'Save Changes' : 'Create User'}
            </Button>
          </div>
        </div>
      }
    >
      <form onSubmit={handleSubmit} className="space-y-4">
        <div className="grid grid-cols-2 gap-4">
          <Input
            label="First Name"
            required
            value={formData.firstName}
            onChange={(e) => setFormData({ ...formData, firstName: e.target.value })}
            error={errors.firstName}
          />
          <Input
            label="Last Name"
            required
            value={formData.lastName}
            onChange={(e) => setFormData({ ...formData, lastName: e.target.value })}
            error={errors.lastName}
          />
        </div>

        <Input
          label="Email"
          type="email"
          required
          value={formData.email}
          onChange={(e) => setFormData({ ...formData, email: e.target.value })}
          error={errors.email}
        />

        <Input
          label="Username"
          required
          value={formData.username}
          onChange={(e) => setFormData({ ...formData, username: e.target.value })}
          error={errors.username}
        />

        {!user && (
          <Input
            label="Password"
            type="password"
            required
            value={(formData as CreateUserInput).password}
            onChange={(e) =>
              setFormData({ ...formData, password: e.target.value } as CreateUserInput)
            }
            error={errors.password}
            helperText="Minimum 8 characters"
          />
        )}

        <div className="grid grid-cols-2 gap-4">
          <Select
            label="Role"
            required
            value={formData.role}
            onChange={(e) => setFormData({ ...formData, role: e.target.value as UserRole })}
            options={roleOptions}
          />

          {user && (
            <Select
              label="Status"
              required
              value={(formData as UpdateUserInput).status}
              onChange={(e) =>
                setFormData({ ...formData, status: e.target.value as UserStatus })
              }
              options={statusOptions}
            />
          )}
        </div>
      </form>
    </Modal>
  );
};
