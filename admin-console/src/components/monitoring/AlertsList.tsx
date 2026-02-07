import React from 'react';
import { AlertCircle, Info, AlertTriangle, XCircle } from 'lucide-react';
import { Alert } from '../../types/monitoring';
import { Badge } from '../common/Badge';

export const AlertsList: React.FC = () => {
  // Mock alerts data
  const alerts: Alert[] = [
    {
      id: '1',
      severity: 'warning',
      message: 'High memory usage detected',
      metric: 'memory',
      value: 85,
      timestamp: new Date(Date.now() - 300000).toISOString(),
      acknowledged: false,
    },
    {
      id: '2',
      severity: 'info',
      message: 'Database backup completed',
      metric: 'backup',
      value: 100,
      timestamp: new Date(Date.now() - 600000).toISOString(),
      acknowledged: true,
    },
    {
      id: '3',
      severity: 'error',
      message: 'API response time exceeded threshold',
      metric: 'api_latency',
      value: 2500,
      timestamp: new Date(Date.now() - 900000).toISOString(),
      acknowledged: false,
    },
  ];

  const getSeverityIcon = (severity: Alert['severity']) => {
    switch (severity) {
      case 'info':
        return <Info className="w-5 h-5 text-blue-500" />;
      case 'warning':
        return <AlertTriangle className="w-5 h-5 text-yellow-500" />;
      case 'error':
        return <AlertCircle className="w-5 h-5 text-red-500" />;
      case 'critical':
        return <XCircle className="w-5 h-5 text-red-600" />;
    }
  };

  const getSeverityBadge = (severity: Alert['severity']) => {
    const variants = {
      info: 'info' as const,
      warning: 'warning' as const,
      error: 'danger' as const,
      critical: 'danger' as const,
    };
    return (
      <Badge variant={variants[severity]}>
        {severity.charAt(0).toUpperCase() + severity.slice(1)}
      </Badge>
    );
  };

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h3 className="text-lg font-semibold text-gray-900 mb-4">Recent Alerts</h3>
      <div className="space-y-3">
        {alerts.map((alert) => (
          <div
            key={alert.id}
            className={`border rounded-lg p-3 ${
              alert.acknowledged ? 'bg-gray-50 border-gray-200' : 'bg-white border-gray-300'
            }`}
          >
            <div className="flex items-start justify-between mb-2">
              <div className="flex items-start gap-3">
                {getSeverityIcon(alert.severity)}
                <div>
                  <p className="font-medium text-gray-900">{alert.message}</p>
                  <p className="text-sm text-gray-500 mt-1">
                    {new Date(alert.timestamp).toLocaleString()}
                  </p>
                </div>
              </div>
              {getSeverityBadge(alert.severity)}
            </div>
            {!alert.acknowledged && (
              <button className="text-sm text-blue-600 hover:text-blue-700 ml-8">
                Acknowledge
              </button>
            )}
          </div>
        ))}
      </div>
    </div>
  );
};
