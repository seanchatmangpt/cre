import React from 'react';
import { NavLink } from 'react-router-dom';
import { Users, Settings, Activity, FileText, LayoutDashboard } from 'lucide-react';
import clsx from 'clsx';

export interface MenuItem {
  label: string;
  path: string;
  icon: React.ReactNode;
}

const menuItems: MenuItem[] = [
  { label: 'Dashboard', path: '/', icon: <LayoutDashboard className="w-5 h-5" /> },
  { label: 'User Management', path: '/users', icon: <Users className="w-5 h-5" /> },
  { label: 'Configuration', path: '/configuration', icon: <Settings className="w-5 h-5" /> },
  { label: 'Monitoring', path: '/monitoring', icon: <Activity className="w-5 h-5" /> },
  { label: 'Audit Logs', path: '/audit-logs', icon: <FileText className="w-5 h-5" /> },
];

export interface SidebarProps {
  isOpen: boolean;
}

export const Sidebar: React.FC<SidebarProps> = ({ isOpen }) => {
  return (
    <aside
      className={clsx(
        'fixed left-0 top-16 h-[calc(100vh-4rem)] bg-gray-900 text-white transition-all duration-300 z-30',
        isOpen ? 'w-64' : 'w-0 overflow-hidden'
      )}
    >
      <nav className="flex flex-col gap-1 p-4">
        {menuItems.map((item) => (
          <NavLink
            key={item.path}
            to={item.path}
            end={item.path === '/'}
            className={({ isActive }) =>
              clsx(
                'flex items-center gap-3 px-4 py-3 rounded-lg transition-colors',
                isActive
                  ? 'bg-blue-600 text-white'
                  : 'text-gray-300 hover:bg-gray-800 hover:text-white'
              )
            }
          >
            {item.icon}
            <span className="font-medium">{item.label}</span>
          </NavLink>
        ))}
      </nav>
    </aside>
  );
};
