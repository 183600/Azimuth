#!/bin/bash

# 只测试简单集成测试的脚本

echo "运行简单集成测试..."

# 创建一个临时的moon.pkg.json文件，只包含我们的测试
cd /home/runner/work/Azimuth/Azimuth/src

# 备份原始的moon.pkg.json
cp moon.pkg.json moon.pkg.json.bak

# 创建一个新的moon.pkg.json，只包含我们的测试
cat > moon.pkg.json << EOF
{
  "name": "azimuth",
  "version": "0.1.0",
  "description": "Azimuth telemetry system main package with simple tests",
  "target": "wasm-gc",
  "source": ["."],
  "deps": {
    "moonbitlang/core/assertion": "*",
    "azimuth/telemetry": "*"
  },
  "test": {
    "test": ["azimuth_simple_integration_tests.mbt"]
  }
}
EOF

# 尝试运行测试
echo "尝试运行简单测试..."
moon test

# 恢复原始的moon.pkg.json
mv moon.pkg.json.bak moon.pkg.json

echo "测试完成"