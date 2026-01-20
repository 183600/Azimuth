// 标准综合测试套件 - 为 Azimuth 项目添加的新测试用例

// 测试加法函数的边界情况
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
}

// 测试乘法函数的边界情况
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与边界值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

// 测试字符串处理函数
test "greet_function_variations" {
  // 测试不同类型的输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, Moon Bit!", @azimuth.greet("Moon Bit"))
}

// 测试向上取整除法函数
test "divide_with_ceil_functionality" {
  // 测试正常情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(6, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(4, 5))
  
  // 测试负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-4, 5))
  
  // 测试整除情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(6, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
}

// 测试布尔断言函数
test "boolean_assertions" {
  // 测试 assert_true
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 1)
  
  // 测试 assert_false
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(1 < 0)
}

// 测试复合运算
test "compound_operations" {
  // 测试加法和乘法的组合
  let a = 2
  let b = 3
  let c = 4
  
  @azimuth.assert_eq(20, @azimuth.multiply(@azimuth.add(a, b), c))
  @azimuth.assert_eq(14, @azimuth.add(@azimuth.multiply(a, b), c))
  @azimuth.assert_eq(9, @azimuth.add(@azimuth.multiply(a, c), b))
}

// 测试字符串断言函数
test "string_assertions" {
  // 测试相等字符串
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("测试中文", "测试中文")
  @azimuth.assert_eq_string("!@#$%^&*()", "!@#$%^&*()")
}

// 测试边界值的除法
test "divide_with_ceil_edge_cases" {
  // 测试除数为1的情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(5, 1))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-5, 1))
  
  // 测试被除数为0的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -3))
  
  // 测试大数值
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(500001, @azimuth.divide_with_ceil(1000001, 2))
}

// 测试函数链式调用
test "function_chaining" {
  // 测试函数的链式调用
  let result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result)
  
  let result2 = @azimuth.multiply(@azimuth.add(10, 5), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, result2)
}

// 测试综合场景
test "comprehensive_scenario" {
  // 模拟一个综合计算场景
  let base_value = 100
  let multiplier = 2
  let increment = 50
  
  // 先乘法再加法
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.add(step1, increment)
  
  @azimuth.assert_eq(250, step2)
  
  // 验证断言函数
  @azimuth.assert_true(step2 > base_value)
  @azimuth.assert_false(step2 < base_value)
  
  // 测试字符串输出
  let message = @azimuth.greet("Result")
  @azimuth.assert_eq_string("Hello, Result!", message)
}