import React from 'react';
import { Users, Activity, FileText, Settings } from 'lucide-react';
import { Link } from 'react-router-dom';

interface DashboardCard {
  title: string;
  value: string;
  change: string;
  icon: React.ReactNode;
  link: string;
  color: string;
}

export const Dashboard: React.FC = () => {
  const cards: DashboardCard[] = [
    {
      title: 'Total Users',
      value: '2,543',
      change: '+12% from last month',
      icon: <Users className="w-8 h-8" />,
      link: '/users',
      color: 'blue',
    },
    {
      title: 'Active Sessions',
      value: '847',
      change: '+5% from last hour',
      icon: <Activity className="w-8 h-8" />,
      link: '/monitoring',
      color: 'green',
    },
    {
      title: 'Audit Logs',
      value: '15,234',
      change: 'Last 24 hours',
      icon: <FileText className="w-8 h-8" />,
      link: '/audit-logs',
      color: 'purple',
    },
    {
      title: 'System Health',
      value: '99.9%',
      change: 'All systems operational',
      icon: <Settings className="w-8 h-8" />,
      link: '/monitoring',
      color: 'orange',
    },
  ];

  const colorClasses: Record<string, string> = {
    blue: 'bg-blue-100 text-blue-600',
    green: 'bg-green-100 text-green-600',
    purple: 'bg-purple-100 text-purple-600',
    orange: 'bg-orange-100 text-orange-600',
  };

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-gray-900">Dashboard</h1>
        <p className="text-gray-600 mt-1">Welcome to the Admin Console</p>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
        {cards.map((card) => (
          <Link
            key={card.title}
            to={card.link}
            className="bg-white rounded-lg shadow p-6 hover:shadow-lg transition-shadow"
          >
            <div className="flex items-center justify-between mb-4">
              <div className={`p-3 rounded-lg ${colorClasses[card.color]}`}>
                {card.icon}
              </div>
            </div>
            <h3 className="text-sm font-medium text-gray-600 mb-1">{card.title}</h3>
            <p className="text-2xl font-bold text-gray-900 mb-2">{card.value}</p>
            <p className="text-sm text-gray-500">{card.change}</p>
          </Link>
        ))}
      </div>

      <div className="bg-white rounded-lg shadow p-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-4">Quick Actions</h2>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <Link
            to="/users"
            className="p-4 border border-gray-200 rounded-lg hover:bg-gray-50 transition-colors"
          >
            <Users className="w-6 h-6 text-blue-600 mb-2" />
            <h3 className="font-medium text-gray-900">Manage Users</h3>
            <p className="text-sm text-gray-600 mt-1">Add, edit, or remove user accounts</p>
          </Link>
          <Link
            to="/configuration"
            className="p-4 border border-gray-200 rounded-lg hover:bg-gray-50 transition-colors"
          >
            <Settings className="w-6 h-6 text-green-600 mb-2" />
            <h3 className="font-medium text-gray-900">System Settings</h3>
            <p className="text-sm text-gray-600 mt-1">Configure system parameters</p>
          </Link>
          <Link
            to="/audit-logs"
            className="p-4 border border-gray-200 rounded-lg hover:bg-gray-50 transition-colors"
          >
            <FileText className="w-6 h-6 text-purple-600 mb-2" />
            <h3 className="font-medium text-gray-900">View Logs</h3>
            <p className="text-sm text-gray-600 mt-1">Review system audit logs</p>
          </Link>
        </div>
      </div>
    </div>
  );
};
