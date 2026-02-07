import React from 'react';
import { CheckCircle, AlertTriangle, XCircle } from 'lucide-react';
import { ServiceStatus as ServiceStatusType } from '../../types/monitoring';
import { Badge } from '../common/Badge';

export interface ServiceStatusProps {
  services: ServiceStatusType[];
}

export const ServiceStatus: React.FC<ServiceStatusProps> = ({ services }) => {
  const getStatusIcon = (status: ServiceStatusType['status']) => {
    switch (status) {
      case 'healthy':
        return <CheckCircle className="w-5 h-5 text-green-500" />;
      case 'degraded':
        return <AlertTriangle className="w-5 h-5 text-yellow-500" />;
      case 'down':
        return <XCircle className="w-5 h-5 text-red-500" />;
    }
  };

  const getStatusBadge = (status: ServiceStatusType['status']) => {
    const variants = {
      healthy: 'success' as const,
      degraded: 'warning' as const,
      down: 'danger' as const,
    };
    return (
      <Badge variant={variants[status]}>
        {status.charAt(0).toUpperCase() + status.slice(1)}
      </Badge>
    );
  };

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h3 className="text-lg font-semibold text-gray-900 mb-4">Service Status</h3>
      <div className="space-y-4">
        {services.map((service) => (
          <div key={service.name} className="border-b border-gray-200 pb-4 last:border-0 last:pb-0">
            <div className="flex items-center justify-between mb-2">
              <div className="flex items-center gap-3">
                {getStatusIcon(service.status)}
                <div>
                  <h4 className="font-medium text-gray-900">{service.name}</h4>
                  <p className="text-sm text-gray-500">
                    Uptime: {service.uptime}%
                  </p>
                </div>
              </div>
              {getStatusBadge(service.status)}
            </div>
            <div className="flex items-center gap-4 text-sm text-gray-600 ml-8">
              {service.responseTime && (
                <span>Response: {service.responseTime}ms</span>
              )}
              <span>
                Last check: {new Date(service.lastCheck).toLocaleTimeString()}
              </span>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};
