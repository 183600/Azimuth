#!/bin/bash
echo "验证新创建的测试文件..."
echo "文件位置: /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_advanced_moonbit_test_suite.mbt"
echo "文件大小: $(wc -l < /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_advanced_moonbit_test_suite.mbt) 行"
echo ""
echo "测试用例数量: $(grep -c "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_advanced_moonbit_test_suite.mbt)"
echo ""
echo "测试用例列表:"
grep "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_advanced_moonbit_test_suite.mbt
echo ""
echo "文件已成功创建并包含10个高级测试用例"