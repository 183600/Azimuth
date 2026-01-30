// 新增的标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含8个高质量测试用例，使用标准的 MoonBit 测试语法

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  subtract(10, 3) = 7
  subtract(0, 0) = 0
  subtract(-5, -3) = -2
  subtract(5, -3) = 8
  subtract(-5, 3) = -8
  subtract(100, 100) = 0
}

test "arithmetic_operation_chaining" {
  // 测试算术运算链式组合
  add(multiply(2, 3), subtract(10, 4)) = 12  // 2*3 + (10-4) = 6 + 6 = 12
  
  divide_with_ceil(
    subtract(multiply(5, 4), add(3, 2)),
    multiply(2, 1)
  ) = 8  // ceil((20-5)/2) = ceil(15/2) = 8
}

test "greet_function_variations" {
  // 测试问候函数的各种输入
  greet("") = "Hello, !"
  greet(" ") = "Hello,  !"
  greet("123") = "Hello, 123!"
  greet("!@#$%") = "Hello, !@#$%!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边缘情况
  divide_with_ceil(10, 0) = 0
  divide_with_ceil(0, 5) = 0
  divide_with_ceil(1, 1) = 1
  divide_with_ceil(-1, -1) = 1
  divide_with_ceil(0, -1) = 0
}

test "mathematical_properties" {
  // 测试数学恒等式性质
  add(5, 0) = 5
  multiply(5, 1) = 5
  multiply(5, 0) = 0
  subtract(5, 5) = 0
  multiply(42, 0) = 0
  subtract(42, 0) = 42
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  multiply(8, 5) = 40
  multiply(40, 2) = 80
  divide_with_ceil(150, 80) = 2
  subtract(multiply(2, 80), 150) = 10
}

test "business_calculation_scenario" {
  // 业务计算场景：库存管理
  subtract(150, 47) = 103  // 剩余库存
  add(103, 80) = 183        // 新到货后总库存
  divide_with_ceil(183, 6) = 31  // 需要的箱子数量
  subtract(multiply(31, 6), 183) = 3  // 剩余空间
}

test "mathematical_laws_verification" {
  // 验证数学定律
  add(7, 11) = add(11, 7)  // 加法交换律
  multiply(7, 11) = multiply(11, 7)  // 乘法交换律
  add(add(7, 11), 5) = add(7, add(11, 5))  // 加法结合律
  multiply(multiply(7, 11), 5) = multiply(7, multiply(11, 5))  // 乘法结合律
}