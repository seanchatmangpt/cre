import React from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts';
import { format } from 'date-fns';
import { MetricPoint } from '../../types/monitoring';

export interface SystemChartProps {
  title: string;
  data: MetricPoint[];
  color: string;
}

export const SystemChart: React.FC<SystemChartProps> = ({ title, data, color }) => {
  const chartData = data.map((point) => ({
    time: format(new Date(point.timestamp), 'HH:mm'),
    value: Math.round(point.value),
  }));

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h3 className="text-lg font-semibold text-gray-900 mb-4">{title}</h3>
      <ResponsiveContainer width="100%" height={250}>
        <LineChart data={chartData}>
          <CartesianGrid strokeDasharray="3 3" stroke="#E5E7EB" />
          <XAxis
            dataKey="time"
            stroke="#6B7280"
            style={{ fontSize: '12px' }}
            tickLine={false}
          />
          <YAxis
            stroke="#6B7280"
            style={{ fontSize: '12px' }}
            tickLine={false}
            tickFormatter={(value) => `${value}%`}
          />
          <Tooltip
            contentStyle={{
              backgroundColor: '#FFF',
              border: '1px solid #E5E7EB',
              borderRadius: '0.5rem',
            }}
            formatter={(value) => [`${value}%`, 'Usage']}
          />
          <Line
            type="monotone"
            dataKey="value"
            stroke={color}
            strokeWidth={2}
            dot={false}
            activeDot={{ r: 6 }}
          />
        </LineChart>
      </ResponsiveContainer>
    </div>
  );
};
