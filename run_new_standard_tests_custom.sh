#!/bin/bash
echo "运行新创建的标准 MoonBit 测试用例..."
cd /home/runner/work/Azimuth/Azimuth

# 检查是否存在 moon 可执行文件
if [ ! -f "./moon" ]; then
    echo "MoonBit 可执行文件不存在，尝试解压..."
    
    # 如果有 moon 压缩包，解压它
    if [ -f "moon.tar.gz" ]; then
        tar -xzf moon.tar.gz
        echo "MoonBit 已解压"
    elif [ -f "moonbit.tar.gz" ]; then
        tar -xzf moonbit.tar.gz
        echo "MoonBit 已解压"
    else
        echo "找不到 MoonBit 可执行文件或压缩包"
        exit 1
    fi
fi

# 检查 moon 命令是否可用
if command -v ./moon &> /dev/null; then
    MOON_CMD="./moon"
elif command -v moon &> /dev/null; then
    MOON_CMD="moon"
else
    echo "无法找到 moon 命令"
    exit 1
fi

echo "使用 MoonBit 命令: $MOON_CMD"

# 备份原始配置文件
cp azimuth/moon.pkg.json azimuth/moon.pkg.json.backup 2>/dev/null || true

# 创建简化的配置文件，包含我们的新测试
cat > azimuth/moon.pkg.json << 'EOF'
{
  "name": "azimuth",
  "export": ["add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false", "divide_with_ceil", "subtract"],
  "files": ["lib.mbt", "../new_standard_tests.mbt"],
  "import": ["moonbitlang/core/builtin", "moonbitlang/core"],
  "test-import": ["moonbitlang/core/builtin", "azimuth"],
  "link": {
    "azimuth/test": "self"
  }
}
EOF

echo "配置文件已创建，开始运行测试..."
$MOON_CMD test

# 恢复原始配置文件
if [ -f azimuth/moon.pkg.json.backup ]; then
    mv azimuth/moon.pkg.json.backup azimuth/moon.pkg.json
    echo "原始配置文件已恢复"
fi

echo "测试完成"