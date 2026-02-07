import React, { useState } from 'react';
import { Search, Filter, Download } from 'lucide-react';
import { DataTable, Column } from '../common/DataTable';
import { Pagination } from '../common/Pagination';
import { Button } from '../common/Button';
import { Input } from '../common/Input';
import { Badge } from '../common/Badge';
import { AuditLog, AuditAction, AuditSeverity } from '../../types/audit';
import { AuditFiltersModal } from './AuditFiltersModal';
import { AuditDetailsModal } from './AuditDetailsModal';
import { format } from 'date-fns';

export const AuditLogs: React.FC = () => {
  const [searchTerm, setSearchTerm] = useState('');
  const [showFiltersModal, setShowFiltersModal] = useState(false);
  const [showDetailsModal, setShowDetailsModal] = useState(false);
  const [selectedLog, setSelectedLog] = useState<AuditLog | null>(null);
  const [currentPage, setCurrentPage] = useState(1);
  const [pageSize, setPageSize] = useState(25);
  const [sortBy, setSortBy] = useState<keyof AuditLog>('timestamp');
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('desc');

  // Mock data
  const mockLogs: AuditLog[] = [
    {
      id: '1',
      timestamp: '2024-02-06T10:30:00Z',
      userId: 'user-1',
      username: 'john.doe',
      action: AuditAction.LOGIN,
      resource: 'auth',
      severity: AuditSeverity.INFO,
      ipAddress: '192.168.1.100',
      userAgent: 'Mozilla/5.0...',
      status: 'success',
    },
    {
      id: '2',
      timestamp: '2024-02-06T10:25:00Z',
      userId: 'user-2',
      username: 'admin',
      action: AuditAction.UPDATE,
      resource: 'user',
      resourceId: 'user-123',
      severity: AuditSeverity.WARNING,
      ipAddress: '192.168.1.101',
      userAgent: 'Mozilla/5.0...',
      status: 'success',
      changes: {
        before: { role: 'user' },
        after: { role: 'admin' },
      },
    },
    {
      id: '3',
      timestamp: '2024-02-06T10:20:00Z',
      userId: 'user-3',
      username: 'jane.smith',
      action: AuditAction.DELETE,
      resource: 'document',
      resourceId: 'doc-456',
      severity: AuditSeverity.CRITICAL,
      ipAddress: '192.168.1.102',
      userAgent: 'Mozilla/5.0...',
      status: 'success',
    },
  ];

  const columns: Column<AuditLog>[] = [
    {
      key: 'timestamp',
      label: 'Timestamp',
      sortable: true,
      width: '180px',
      render: (value) => format(new Date(String(value)), 'MMM dd, yyyy HH:mm:ss'),
    },
    {
      key: 'username',
      label: 'User',
      sortable: true,
    },
    {
      key: 'action',
      label: 'Action',
      sortable: true,
      render: (value) => {
        const actionColors: Record<AuditAction, 'info' | 'warning' | 'danger' | 'success'> = {
          [AuditAction.CREATE]: 'success',
          [AuditAction.READ]: 'info',
          [AuditAction.UPDATE]: 'warning',
          [AuditAction.DELETE]: 'danger',
          [AuditAction.LOGIN]: 'info',
          [AuditAction.LOGOUT]: 'info',
          [AuditAction.PERMISSION_CHANGE]: 'warning',
          [AuditAction.CONFIG_CHANGE]: 'warning',
          [AuditAction.EXPORT]: 'info',
          [AuditAction.IMPORT]: 'info',
        };
        return (
          <Badge variant={actionColors[value as AuditAction]}>
            {String(value).replace('_', ' ').toUpperCase()}
          </Badge>
        );
      },
    },
    {
      key: 'resource',
      label: 'Resource',
      sortable: true,
    },
    {
      key: 'severity',
      label: 'Severity',
      sortable: true,
      render: (value) => {
        const severityColors: Record<AuditSeverity, 'info' | 'warning' | 'danger'> = {
          [AuditSeverity.INFO]: 'info',
          [AuditSeverity.WARNING]: 'warning',
          [AuditSeverity.ERROR]: 'danger',
          [AuditSeverity.CRITICAL]: 'danger',
        };
        return (
          <Badge variant={severityColors[value as AuditSeverity]}>
            {String(value).toUpperCase()}
          </Badge>
        );
      },
    },
    {
      key: 'status',
      label: 'Status',
      sortable: true,
      render: (value) => (
        <Badge variant={value === 'success' ? 'success' : 'danger'}>
          {String(value).toUpperCase()}
        </Badge>
      ),
    },
    {
      key: 'ipAddress',
      label: 'IP Address',
      sortable: false,
    },
  ];

  const handleSort = (key: string, order: 'asc' | 'desc') => {
    setSortBy(key as keyof AuditLog);
    setSortOrder(order);
  };

  const handleRowClick = (log: AuditLog) => {
    setSelectedLog(log);
    setShowDetailsModal(true);
  };

  const handleExport = () => {
    console.log('Exporting audit logs...');
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h1 className="text-2xl font-bold text-gray-900">Audit Logs</h1>
        <Button
          variant="secondary"
          leftIcon={<Download className="w-4 h-4" />}
          onClick={handleExport}
        >
          Export
        </Button>
      </div>

      <div className="bg-white rounded-lg shadow">
        <div className="p-4 border-b border-gray-200">
          <div className="flex gap-4">
            <div className="flex-1">
              <Input
                placeholder="Search logs..."
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
          data={mockLogs}
          columns={columns}
          onSort={handleSort}
          sortBy={sortBy}
          sortOrder={sortOrder}
          onRowClick={handleRowClick}
        />

        <Pagination
          currentPage={currentPage}
          totalPages={20}
          onPageChange={setCurrentPage}
          pageSize={pageSize}
          totalItems={500}
          onPageSizeChange={setPageSize}
        />
      </div>

      <AuditFiltersModal
        isOpen={showFiltersModal}
        onClose={() => setShowFiltersModal(false)}
      />

      <AuditDetailsModal
        isOpen={showDetailsModal}
        onClose={() => setShowDetailsModal(false)}
        log={selectedLog}
      />
    </div>
  );
};
