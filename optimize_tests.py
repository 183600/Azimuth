#!/usr/bin/env python3
"""
Optimize test files by removing unnecessary initTelemetry and shutdownTelemetry calls
"""

import os
import re
import glob

def optimize_test_file(file_path):
    """Optimize a single test file by removing unnecessary initTelemetry and shutdownTelemetry calls"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Remove initTelemetry productionConfig calls in QuickCheck properties
        # Pattern: initTelemetry productionConfig followed by newline and indentation
        content = re.sub(
            r'(\s+)initTelemetry productionConfig\n',
            r'\1',
            content
        )
        
        # Remove shutdownTelemetry calls in QuickCheck properties
        # Pattern: shutdownTelemetry followed by newline and indentation
        content = re.sub(
            r'(\s+)shutdownTelemetry\n',
            r'\1',
            content
        )
        
        # Write back if changed
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Optimized: {file_path}")
            return True
        else:
            print(f"No changes needed: {file_path}")
            return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    # Find all test spec files
    test_files = glob.glob('/home/runner/work/Azimuth/Azimuth/test/*Spec.hs')
    
    optimized_count = 0
    for file_path in test_files:
        if optimize_test_file(file_path):
            optimized_count += 1
    
    print(f"\nOptimized {optimized_count} test files")

if __name__ == "__main__":
    main()