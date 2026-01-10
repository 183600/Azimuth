# Azimuth - MoonBit 项目

这是一个基本的 MoonBit 项目结构示例。

## 项目结构

```
.
├── moon.mod.json          # 项目配置文件
├── src/
│   └── azimuth/
│       ├── lib.mbt        # 主要源代码文件
│       └── test/
│           └── lib_test.mbt # 测试文件
└── README.md              # 项目说明
```

## 安装 MoonBit

在使用此项目之前，请先安装 MoonBit：

```bash
# 安装 MoonBit CLI
curl -fsSL https://moonbitlang.com/install | bash
```

## 运行测试

安装完成后，可以使用以下命令运行测试：

```bash
moon test
```

## 项目功能

当前项目包含以下基本功能：

- `add(a, b)`: 两个整数相加
- `multiply(a, b)`: 两个整数相乘
- `greet(name)`: 生成问候语

## 开发指南

1. 在 `src/azimuth/` 目录下添加新的 `.mbt` 源文件
2. 在 `src/azimuth/test/` 目录下添加对应的测试文件
3. 使用 `moon test` 运行所有测试
4. 使用 `moon run` 运行项目（如果配置了入口点）