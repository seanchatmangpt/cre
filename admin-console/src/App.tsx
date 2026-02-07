import React from 'react';
import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { Layout } from './components/layout/Layout';
import { UserManagement } from './components/user-management/UserManagement';
import { Configuration } from './components/configuration/Configuration';
import { Monitoring } from './components/monitoring/Monitoring';
import { AuditLogs } from './components/audit-logs/AuditLogs';
import { Dashboard } from './components/dashboard/Dashboard';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      refetchOnWindowFocus: false,
      retry: 1,
      staleTime: 5 * 60 * 1000,
    },
  },
});

function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <Routes>
          <Route path="/" element={<Layout />}>
            <Route index element={<Dashboard />} />
            <Route path="users" element={<UserManagement />} />
            <Route path="configuration" element={<Configuration />} />
            <Route path="monitoring" element={<Monitoring />} />
            <Route path="audit-logs" element={<AuditLogs />} />
            <Route path="*" element={<Navigate to="/" replace />} />
          </Route>
        </Routes>
      </BrowserRouter>
    </QueryClientProvider>
  );
}

export default App;
