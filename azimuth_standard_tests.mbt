// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个测试用例

// 导入必要的模块
use azimuth

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-1, 1))
  assert_eq(-10, add(-5, -5))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(20, multiply(-4, -5))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = add(a, b)
  let product = multiply(sum, c)
  let result = add(product, -d)
  
  assert_eq(75, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 5)), add(multiply(x, y), multiply(x, 5)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品价格 × 数量 + 运费
  let item_price = 199
  let quantity = 2
  let shipping = 20
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(418, total)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(min_val, -2) == min_val)
  
  // 断言函数测试
  assert_true(true)
  assert_false(false)
}

test "comprehensive_integration" {
  // 综合集成测试
  // 计算矩形的面积和周长
  let length = 8
  let width = 6
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(48, area)
  assert_eq(28, perimeter)
  
  // 测试字符串拼接与数学运算结合
  let shape_name = "Rectangle"
  let description = greet(shape_name)
  assert_eq_string("Hello, Rectangle!", description)
}