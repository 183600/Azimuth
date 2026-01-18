// Azimuth 项目标准测试套件
// 使用标准 MoonBit 测试语法编写

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-1, 1))
  assert_eq(-3, add(-1, -2))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(0, 0))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 100))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 5
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(150, result)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(205, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形面积和周长
  let length = 10
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(50, area)
  assert_eq(30, perimeter)
}

test "error_handling" {
  // 测试错误处理
  assert_true(true)
  assert_false(false)
  
  // 测试断言函数
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}

test "performance_patterns" {
  // 测试性能模式
  let base = 100
  
  // 重复加法 vs 乘法
  let repeated_add = add(add(add(add(base, base), base), base), base)
  let multiplication = multiply(base, 5)
  
  assert_eq(repeated_add, multiplication)
  
  // 嵌套运算
  let nested = multiply(add(10, 5), add(20, 10))
  assert_eq(225, nested)
}