#!/usr/bin/env python3
"""
Fix syntax errors in test files caused by the optimization script
"""

import os
import re
import glob

def fix_syntax_errors(file_path):
    """Fix syntax errors in a test file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix pattern: "finalValue <- metricValue\n                      metric return (finalValue == ..."
        content = re.sub(
            r'(\s+)finalValue <- metricValue\n\s+metric return \(finalValue ==',
            r'\1finalValue <- metricValue metric\n\1return (finalValue ==',
            content
        )
        
        # Fix pattern: "result <- try\n                  $ case result of"
        content = re.sub(
            r'(\s+)result <- try\n\s+\$ case result of',
            r'\1result <- try $ do\n\1  case result of',
            content
        )
        
        # Fix pattern: "result <- (try :: IO a -> IO \(Either SomeException a\))\n                  \$ do metric <- createMetric"
        content = re.sub(
            r'(\s+)result <- \(try :: IO a -> IO \(Either SomeException a\)\)\n\s+\$ do metric <- createMetric',
            r'\1result <- (try :: IO a -> IO (Either SomeException a)) $ do\n\1  metric <- createMetric',
            content
        )
        
        # Fix pattern: "let _ = result :: Either SomeException \(\) return \(isSuccess result\)"
        content = re.sub(
            r'(\s+)let _ = result :: Either SomeException \(\) return \(isSuccess result\)',
            r'\1let _ = result :: Either SomeException ()\n\1return (isSuccess result)',
            content
        )
        
        # Fix pattern: "performGC\n          performGC"
        content = re.sub(
            r'(\s+)performGC\n\s+performGC',
            r'\1performGC',
            content
        )
        
        # Fix pattern: "loggerResult <- try \(do\n                        \(do when logging"
        content = re.sub(
            r'(\s+)loggerResult <- try \(do\n\s+\(do when logging',
            r'\1loggerResult <- try $ do\n\1  when logging',
            content
        )
        
        # Write back if changed
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed: {file_path}")
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
    
    fixed_count = 0
    for file_path in test_files:
        if fix_syntax_errors(file_path):
            fixed_count += 1
    
    print(f"\nFixed {fixed_count} test files")

if __name__ == "__main__":
    main()