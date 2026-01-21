// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能测试
// 包含基本运算、字符串处理和边界情况测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合使用
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5+3)*2 = 8*2 = 16
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "business_calculation_scenario" {
  // 测试业务计算场景：购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
}

test "packaging_calculation" {
  // 测试包装计算场景：计算需要的包装箱数量
  let total_items = 23
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(23/5) = 5
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算
  let a = 10
  let b = 20
  let c = 3
  
  // 计算：(a + b) * c - a / b
  let result = add(multiply(add(a, b), c), -divide_with_ceil(a, b))
  assert_eq(89, result)  // (10 + 20) * 3 - ceil(10/20) = 30*3 - 1 = 90 - 1 = 89
}