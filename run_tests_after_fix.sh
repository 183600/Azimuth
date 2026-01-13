#!/bin/bash

# 运行测试并保存结果
echo "Running tests after fixing compilation errors..."

# 创建日志文件
LOG_FILE="/home/runner/work/Azimuth/Azimuth/test_run_after_fix.log"

# 运行测试并保存输出
echo "Running moon test..." > $LOG_FILE
./moon test >> $LOG_FILE 2>&1

# 显示结果
echo "Test results saved to: $LOG_FILE"
echo "Last 20 lines of test output:"
tail -20 $LOG_FILE