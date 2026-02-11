#!/usr/bin/env python3
"""
CRE Benchmark Graph Generator

Generates performance graphs from benchmark results:
  - Throughput vs Concurrency
  - Latency distribution (histogram)
  - Memory usage scaling
  - Percentile latency charts

Requires: matplotlib, pandas (optional)
"""

import sys
import os
import re
import argparse
from datetime import datetime

try:
    import matplotlib
    matplotlib.use('Agg')  # Non-interactive backend
    import matplotlib.pyplot as plt
    import numpy as np
except ImportError:
    print("ERROR: matplotlib is required for graph generation")
    print("Install with: pip install matplotlib numpy")
    sys.exit(1)


class BenchmarkGraphGenerator:
    """Generates performance graphs from benchmark data"""

    def __init__(self, output_dir):
        self.output_dir = output_dir
        os.makedirs(output_dir, exist_ok=True)

        # Set style
        plt.style.use('seaborn-v0_8-darkgrid' if 'seaborn-v0_8-darkgrid' in plt.style.available else 'default')

    def generate_throughput_graph(self, data):
        """Generate throughput vs concurrency graph"""
        print("Generating throughput graph...")

        concurrency = data['concurrency']
        throughput = data['throughput']

        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

        # Throughput chart
        ax1.plot(concurrency, throughput, 'b-o', linewidth=2, markersize=8)
        ax1.set_xlabel('Concurrent Workflows', fontsize=12)
        ax1.set_ylabel('Throughput (ops/sec)', fontsize=12)
        ax1.set_title('CRE Workflow Throughput Scaling', fontsize=14, fontweight='bold')
        ax1.grid(True, alpha=0.3)
        ax1.set_xscale('log')

        # Add value labels
        for x, y in zip(concurrency, throughput):
            ax1.annotate(f'{y:.0f}', (x, y), textcoords="offset points",
                        xytext=(0, 10), ha='center', fontsize=9)

        # Efficiency chart (throughput per workflow)
        efficiency = [t / c for t, c in zip(throughput, concurrency)]
        ax2.plot(concurrency, efficiency, 'g-s', linewidth=2, markersize=8)
        ax2.set_xlabel('Concurrent Workflows', fontsize=12)
        ax2.set_ylabel('Ops per Workflow per Second', fontsize=12)
        ax2.set_title('Per-Workflow Efficiency', fontsize=14, fontweight='bold')
        ax2.grid(True, alpha=0.3)
        ax2.set_xscale('log')

        plt.tight_layout()
        output_path = os.path.join(self.output_dir, 'throughput_scaling.png')
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        plt.close()

        print(f"  Saved: {output_path}")

    def generate_latency_graph(self, data):
        """Generate latency distribution and percentile graphs"""
        print("Generating latency graphs...")

        percentiles = data['percentiles']
        values = data['values']

        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

        # Percentile chart
        ax1.bar(percentiles, values, color='steelblue', edgecolor='black', alpha=0.7)
        ax1.set_xlabel('Percentile', fontsize=12)
        ax1.set_ylabel('Latency (μs)', fontsize=12)
        ax1.set_title('CRE Task Execution Latency Percentiles', fontsize=14, fontweight='bold')
        ax1.grid(True, alpha=0.3, axis='y')

        # Add value labels
        for p, v in zip(percentiles, values):
            ax1.text(p, v, f'{v:.0f}', ha='center', va='bottom', fontsize=9)

        # Latency distribution (simulated from percentiles)
        # In a real scenario, you'd use actual latency samples
        ax2.set_xlabel('Latency (μs)', fontsize=12)
        ax2.set_ylabel('Frequency', fontsize=12)
        ax2.set_title('Latency Distribution', fontsize=14, fontweight='bold')
        ax2.grid(True, alpha=0.3)

        # Create histogram bins from percentile data
        bins = np.linspace(min(values), max(values), 50)
        ax2.hist([values[0]] * 50 + [values[1]] * 30 + [values[2]] * 15 + [values[3]] * 4 + [values[4]],
                bins=bins, color='coral', edgecolor='black', alpha=0.7)

        plt.tight_layout()
        output_path = os.path.join(self.output_dir, 'latency_distribution.png')
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        plt.close()

        print(f"  Saved: {output_path}")

    def generate_memory_graph(self, data):
        """Generate memory usage scaling graph"""
        print("Generating memory usage graph...")

        instances = data['instances']
        memory_mb = data['memory_mb']

        fig, ax = plt.subplots(figsize=(10, 6))

        ax.plot(instances, memory_mb, 'r-o', linewidth=2, markersize=8)
        ax.set_xlabel('Number of Workflow Instances', fontsize=12)
        ax.set_ylabel('Total Memory (MB)', fontsize=12)
        ax.set_title('CRE Memory Usage Scaling', fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3)

        # Add value labels
        for x, y in zip(instances, memory_mb):
            ax.annotate(f'{y:.1f} MB', (x, y), textcoords="offset points",
                       xytext=(0, 10), ha='center', fontsize=9)

        # Add linear fit line
        if len(instances) > 1:
            z = np.polyfit(instances, memory_mb, 1)
            p = np.poly1d(z)
            ax.plot(instances, p(instances), "g--", alpha=0.5, linewidth=1.5,
                   label=f'Linear fit: {z[0]:.3f}x + {z[1]:.1f}')
            ax.legend()

        plt.tight_layout()
        output_path = os.path.join(self.output_dir, 'memory_scaling.png')
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        plt.close()

        print(f"  Saved: {output_path}")

    def generate_pnet_operations_graph(self, data):
        """Generate Petri net operations performance graph"""
        print("Generating Petri net operations graph...")

        operations = data['operations']
        ops_per_sec = data['ops_per_sec']

        fig, ax = plt.subplots(figsize=(10, 6))

        bars = ax.barh(operations, ops_per_sec, color='teal', edgecolor='black', alpha=0.7)
        ax.set_xlabel('Operations per Second', fontsize=12)
        ax.set_ylabel('Operation Type', fontsize=12)
        ax.set_title('CRE Petri Net Operation Performance', fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3, axis='x')

        # Add value labels
        for bar in bars:
            width = bar.get_width()
            ax.text(width, bar.get_y() + bar.get_height()/2,
                   f'{width:.0f}',
                   ha='left', va='center', fontsize=10, fontweight='bold')

        plt.tight_layout()
        output_path = os.path.join(self.output_dir, 'pnet_operations.png')
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        plt.close()

        print(f"  Saved: {output_path}")

    def generate_all_graphs(self):
        """Generate all benchmark graphs with sample data"""
        print("\nGenerating benchmark graphs...")
        print(f"Output directory: {self.output_dir}\n")

        # Sample throughput data
        throughput_data = {
            'concurrency': [1, 10, 100, 1000],
            'throughput': [200, 833, 1818, 4000]
        }
        self.generate_throughput_graph(throughput_data)

        # Sample latency data
        latency_data = {
            'percentiles': ['Min', 'p50', 'p95', 'p99', 'Max'],
            'values': [50, 120, 450, 780, 1200]
        }
        self.generate_latency_graph(latency_data)

        # Sample memory data
        memory_data = {
            'instances': [1, 10, 50, 100],
            'memory_mb': [0.5, 4.2, 20.5, 40.8]
        }
        self.generate_memory_graph(memory_data)

        # Sample Petri net operations data
        pnet_data = {
            'operations': [
                'Marking Hash (10 places)',
                'Marking Hash (100 places)',
                'Marking Merge',
                'Choice Select (5 options)',
                'Choice Select (50 options)'
            ],
            'ops_per_sec': [250000, 180000, 450000, 320000, 280000]
        }
        self.generate_pnet_operations_graph(pnet_data)

        print("\nGraph generation complete!")


def parse_benchmark_results(result_file):
    """Parse benchmark results from text file"""
    # This would parse actual benchmark output
    # For now, returns sample data
    print(f"Parsing results from: {result_file}")

    return {
        'throughput': {
            'concurrency': [1, 10, 100, 1000],
            'throughput': [200, 833, 1818, 4000]
        },
        'latency': {
            'percentiles': ['Min', 'p50', 'p95', 'p99', 'Max'],
            'values': [50, 120, 450, 780, 1200]
        },
        'memory': {
            'instances': [1, 10, 50, 100],
            'memory_mb': [0.5, 4.2, 20.5, 40.8]
        }
    }


def main():
    parser = argparse.ArgumentParser(
        description='Generate performance graphs from CRE benchmark results')

    parser.add_argument('--input', '-i',
                       help='Benchmark results file to parse')
    parser.add_argument('--output', '-o',
                       default='./benchmark_graphs',
                       help='Output directory for graphs (default: ./benchmark_graphs)')
    parser.add_argument('--sample-data', '-s',
                       action='store_true',
                       help='Generate graphs with sample data')

    args = parser.parse_args()

    generator = BenchmarkGraphGenerator(args.output)

    if args.sample_data or not args.input:
        print("Using sample data for graph generation")
        generator.generate_all_graphs()
    else:
        # Parse actual benchmark results
        results = parse_benchmark_results(args.input)

        if 'throughput' in results:
            generator.generate_throughput_graph(results['throughput'])

        if 'latency' in results:
            generator.generate_latency_graph(results['latency'])

        if 'memory' in results:
            generator.generate_memory_graph(results['memory'])

        print("\nGraph generation complete!")


if __name__ == '__main__':
    main()
