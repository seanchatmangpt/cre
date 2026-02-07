import React from 'react';
import { Activity, Cpu, HardDrive, Network } from 'lucide-react';
import { MetricsCard } from './MetricsCard';
import { SystemChart } from './SystemChart';
import { ServiceStatus } from './ServiceStatus';
import { AlertsList } from './AlertsList';

export const Monitoring: React.FC = () => {
  // Mock data
  const systemMetrics = {
    cpu: {
      usage: 45,
      cores: 8,
      history: Array.from({ length: 24 }, (_, i) => ({
        timestamp: new Date(Date.now() - (23 - i) * 3600000).toISOString(),
        value: Math.random() * 100,
      })),
    },
    memory: {
      used: 12.5,
      total: 16,
      percentage: 78,
      history: Array.from({ length: 24 }, (_, i) => ({
        timestamp: new Date(Date.now() - (23 - i) * 3600000).toISOString(),
        value: Math.random() * 100,
      })),
    },
    disk: {
      used: 250,
      total: 500,
      percentage: 50,
    },
    network: {
      incoming: 125.5,
      outgoing: 87.3,
      history: Array.from({ length: 24 }, (_, i) => ({
        timestamp: new Date(Date.now() - (23 - i) * 3600000).toISOString(),
        value: Math.random() * 200,
      })),
    },
  };

  const services = [
    {
      name: 'API Server',
      status: 'healthy' as const,
      uptime: 99.99,
      lastCheck: new Date().toISOString(),
      responseTime: 45,
    },
    {
      name: 'Database',
      status: 'healthy' as const,
      uptime: 99.95,
      lastCheck: new Date().toISOString(),
      responseTime: 12,
    },
    {
      name: 'Cache Server',
      status: 'degraded' as const,
      uptime: 98.5,
      lastCheck: new Date().toISOString(),
      responseTime: 150,
    },
  ];

  return (
    <div className="space-y-6">
      <h1 className="text-2xl font-bold text-gray-900">System Monitoring</h1>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
        <MetricsCard
          title="CPU Usage"
          value={`${systemMetrics.cpu.usage}%`}
          icon={<Cpu className="w-6 h-6" />}
          trend={{ value: 5, isPositive: false }}
          color="blue"
        />
        <MetricsCard
          title="Memory"
          value={`${systemMetrics.memory.percentage}%`}
          subtitle={`${systemMetrics.memory.used}GB / ${systemMetrics.memory.total}GB`}
          icon={<Activity className="w-6 h-6" />}
          trend={{ value: 3, isPositive: true }}
          color="green"
        />
        <MetricsCard
          title="Disk Usage"
          value={`${systemMetrics.disk.percentage}%`}
          subtitle={`${systemMetrics.disk.used}GB / ${systemMetrics.disk.total}GB`}
          icon={<HardDrive className="w-6 h-6" />}
          color="purple"
        />
        <MetricsCard
          title="Network"
          value={`${systemMetrics.network.incoming} MB/s`}
          subtitle={`Out: ${systemMetrics.network.outgoing} MB/s`}
          icon={<Network className="w-6 h-6" />}
          color="orange"
        />
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <SystemChart
          title="CPU Usage (24h)"
          data={systemMetrics.cpu.history}
          color="#3B82F6"
        />
        <SystemChart
          title="Memory Usage (24h)"
          data={systemMetrics.memory.history}
          color="#10B981"
        />
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ServiceStatus services={services} />
        <AlertsList />
      </div>
    </div>
  );
};
