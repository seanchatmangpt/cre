import React from 'react';
import { Menu, Bell, User, LogOut } from 'lucide-react';

export interface HeaderProps {
  onMenuToggle: () => void;
  user?: {
    name: string;
    email: string;
    avatar?: string;
  };
}

export const Header: React.FC<HeaderProps> = ({ onMenuToggle, user }) => {
  return (
    <header className="fixed top-0 left-0 right-0 h-16 bg-white border-b border-gray-200 z-40">
      <div className="flex items-center justify-between h-full px-4">
        <div className="flex items-center gap-4">
          <button
            onClick={onMenuToggle}
            className="p-2 rounded-lg hover:bg-gray-100 transition-colors"
          >
            <Menu className="w-6 h-6" />
          </button>
          <h1 className="text-xl font-bold text-gray-900">Admin Console</h1>
        </div>

        <div className="flex items-center gap-4">
          <button className="relative p-2 rounded-lg hover:bg-gray-100 transition-colors">
            <Bell className="w-6 h-6" />
            <span className="absolute top-1 right-1 w-2 h-2 bg-red-500 rounded-full" />
          </button>

          <div className="flex items-center gap-3 px-3 py-2 rounded-lg hover:bg-gray-100 transition-colors cursor-pointer">
            {user?.avatar ? (
              <img
                src={user.avatar}
                alt={user.name}
                className="w-8 h-8 rounded-full"
              />
            ) : (
              <div className="w-8 h-8 rounded-full bg-blue-600 flex items-center justify-center">
                <User className="w-5 h-5 text-white" />
              </div>
            )}
            <div className="hidden md:block">
              <div className="text-sm font-medium text-gray-900">{user?.name || 'Admin'}</div>
              <div className="text-xs text-gray-500">{user?.email || 'admin@example.com'}</div>
            </div>
          </div>

          <button className="p-2 rounded-lg hover:bg-gray-100 transition-colors text-gray-600">
            <LogOut className="w-5 h-5" />
          </button>
        </div>
      </div>
    </header>
  );
};
