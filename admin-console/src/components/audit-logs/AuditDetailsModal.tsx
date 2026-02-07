import React from 'react';
import { Modal } from '../common/Modal';
import { Badge } from '../common/Badge';
import { AuditLog, AuditAction, AuditSeverity } from '../../types/audit';
import { format } from 'date-fns';

export interface AuditDetailsModalProps {
  isOpen: boolean;
  onClose: () => void;
  log: AuditLog | null;
}

export const AuditDetailsModal: React.FC<AuditDetailsModalProps> = ({
  isOpen,
  onClose,
  log,
}) => {
  if (!log) return null;

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

  const severityColors: Record<AuditSeverity, 'info' | 'warning' | 'danger'> = {
    [AuditSeverity.INFO]: 'info',
    [AuditSeverity.WARNING]: 'warning',
    [AuditSeverity.ERROR]: 'danger',
    [AuditSeverity.CRITICAL]: 'danger',
  };

  const InfoRow: React.FC<{ label: string; value: React.ReactNode }> = ({ label, value }) => (
    <div className="flex py-3 border-b border-gray-200 last:border-0">
      <dt className="text-sm font-medium text-gray-500 w-1/3">{label}</dt>
      <dd className="text-sm text-gray-900 w-2/3">{value}</dd>
    </div>
  );

  return (
    <Modal isOpen={isOpen} onClose={onClose} title="Audit Log Details" size="lg">
      <dl className="divide-y divide-gray-200">
        <InfoRow label="Timestamp" value={format(new Date(log.timestamp), 'PPpp')} />
        <InfoRow label="User" value={`${log.username} (${log.userId})`} />
        <InfoRow
          label="Action"
          value={<Badge variant={actionColors[log.action]}>{log.action.toUpperCase()}</Badge>}
        />
        <InfoRow label="Resource" value={log.resource} />
        {log.resourceId && <InfoRow label="Resource ID" value={log.resourceId} />}
        <InfoRow
          label="Severity"
          value={<Badge variant={severityColors[log.severity]}>{log.severity.toUpperCase()}</Badge>}
        />
        <InfoRow
          label="Status"
          value={
            <Badge variant={log.status === 'success' ? 'success' : 'danger'}>
              {log.status.toUpperCase()}
            </Badge>
          }
        />
        <InfoRow label="IP Address" value={log.ipAddress} />
        <InfoRow label="User Agent" value={log.userAgent} />

        {log.changes && (
          <>
            <div className="py-3 border-b border-gray-200">
              <dt className="text-sm font-medium text-gray-500 mb-2">Changes</dt>
              <dd className="text-sm">
                {log.changes.before && (
                  <div className="mb-2">
                    <span className="font-medium text-gray-700">Before:</span>
                    <pre className="mt-1 p-2 bg-red-50 rounded text-xs overflow-x-auto">
                      {JSON.stringify(log.changes.before, null, 2)}
                    </pre>
                  </div>
                )}
                {log.changes.after && (
                  <div>
                    <span className="font-medium text-gray-700">After:</span>
                    <pre className="mt-1 p-2 bg-green-50 rounded text-xs overflow-x-auto">
                      {JSON.stringify(log.changes.after, null, 2)}
                    </pre>
                  </div>
                )}
              </dd>
            </div>
          </>
        )}

        {log.details && (
          <div className="py-3">
            <dt className="text-sm font-medium text-gray-500 mb-2">Additional Details</dt>
            <dd className="text-sm">
              <pre className="p-2 bg-gray-50 rounded text-xs overflow-x-auto">
                {JSON.stringify(log.details, null, 2)}
              </pre>
            </dd>
          </div>
        )}
      </dl>
    </Modal>
  );
};
