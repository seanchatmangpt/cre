import React, { useState } from 'react';
import { Plus, Search, Filter } from 'lucide-react';
import { DataTable, Column } from '../common/DataTable';
import { Pagination } from '../common/Pagination';
import { Button } from '../common/Button';
import { Input } from '../common/Input';
import { Badge } from '../common/Badge';
import { User, UserRole, UserStatus } from '../../types/user';
import { UserModal } from './UserModal';
import { UserFiltersModal } from './UserFiltersModal';

export const UserManagement: React.FC = () => {
  const [searchTerm, setSearchTerm] = useState('');
  const [showUserModal, setShowUserModal] = useState(false);
  const [showFiltersModal, setShowFiltersModal] = useState(false);
  const [selectedUser, setSelectedUser] = useState<User | null>(null);
  const [currentPage, setCurrentPage] = useState(1);
  const [pageSize, setPageSize] = useState(25);
  const [sortBy, setSortBy] = useState<keyof User>('createdAt');
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('desc');

  // Mock data - replace with API call
  const mockUsers: User[] = [
    {
      id: '1',
      email: 'john.doe@example.com',
      username: 'johndoe',
      firstName: 'John',
      lastName: 'Doe',
      role: UserRole.ADMIN,
      status: UserStatus.ACTIVE,
      createdAt: '2024-01-15T10:00:00Z',
      updatedAt: '2024-01-15T10:00:00Z',
      lastLoginAt: '2024-02-01T09:30:00Z',
    },
    {
      id: '2',
      email: 'jane.smith@example.com',
      username: 'janesmith',
      firstName: 'Jane',
      lastName: 'Smith',
      role: UserRole.MODERATOR,
      status: UserStatus.ACTIVE,
      createdAt: '2024-01-20T14:30:00Z',
      updatedAt: '2024-01-20T14:30:00Z',
      lastLoginAt: '2024-02-02T11:15:00Z',
    },
  ];

  const columns: Column<User>[] = [
    { key: 'username', label: 'Username', sortable: true },
    { key: 'email', label: 'Email', sortable: true },
    {
      key: 'firstName',
      label: 'Name',
      sortable: true,
      render: (_, user) => `${user.firstName} ${user.lastName}`,
    },
    {
      key: 'role',
      label: 'Role',
      sortable: true,
      render: (value) => {
        const roleColors: Record<UserRole, 'default' | 'success' | 'warning' | 'info'> = {
          [UserRole.ADMIN]: 'danger',
          [UserRole.MODERATOR]: 'warning',
          [UserRole.USER]: 'info',
          [UserRole.VIEWER]: 'default',
        };
        return (
          <Badge variant={roleColors[value as UserRole]}>
            {String(value).toUpperCase()}
          </Badge>
        );
      },
    },
    {
      key: 'status',
      label: 'Status',
      sortable: true,
      render: (value) => {
        const statusColors: Record<UserStatus, 'success' | 'warning' | 'danger'> = {
          [UserStatus.ACTIVE]: 'success',
          [UserStatus.INACTIVE]: 'warning',
          [UserStatus.SUSPENDED]: 'danger',
        };
        return (
          <Badge variant={statusColors[value as UserStatus]}>
            {String(value).toUpperCase()}
          </Badge>
        );
      },
    },
    {
      key: 'createdAt',
      label: 'Created',
      sortable: true,
      render: (value) => new Date(String(value)).toLocaleDateString(),
    },
  ];

  const handleSort = (key: string, order: 'asc' | 'desc') => {
    setSortBy(key as keyof User);
    setSortOrder(order);
  };

  const handleRowClick = (user: User) => {
    setSelectedUser(user);
    setShowUserModal(true);
  };

  const handleCreateUser = () => {
    setSelectedUser(null);
    setShowUserModal(true);
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h1 className="text-2xl font-bold text-gray-900">User Management</h1>
        <Button leftIcon={<Plus className="w-4 h-4" />} onClick={handleCreateUser}>
          Add User
        </Button>
      </div>

      <div className="bg-white rounded-lg shadow">
        <div className="p-4 border-b border-gray-200">
          <div className="flex gap-4">
            <div className="flex-1">
              <Input
                placeholder="Search users..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                leftIcon={<Search className="w-4 h-4" />}
              />
            </div>
            <Button
              variant="ghost"
              leftIcon={<Filter className="w-4 h-4" />}
              onClick={() => setShowFiltersModal(true)}
            >
              Filters
            </Button>
          </div>
        </div>

        <DataTable
          data={mockUsers}
          columns={columns}
          onSort={handleSort}
          sortBy={sortBy}
          sortOrder={sortOrder}
          onRowClick={handleRowClick}
        />

        <Pagination
          currentPage={currentPage}
          totalPages={10}
          onPageChange={setCurrentPage}
          pageSize={pageSize}
          totalItems={250}
          onPageSizeChange={setPageSize}
        />
      </div>

      <UserModal
        isOpen={showUserModal}
        onClose={() => setShowUserModal(false)}
        user={selectedUser}
      />

      <UserFiltersModal
        isOpen={showFiltersModal}
        onClose={() => setShowFiltersModal(false)}
      />
    </div>
  );
};
