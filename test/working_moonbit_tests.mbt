// 工作 MoonBit 测试用例
// 基于标准 MoonBit 测试语法的10个高质量测试用例

test "subtract_function_basic" {
  // 基本减法函数测试
  assert_eq(5, subtract(8, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(3, 8))
}

test "add_function_edge_cases" {
  // 加法函数边缘情况测试
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
  assert_eq(0, add(-500, 500))
}

test "multiply_function_zero_cases" {
  // 乘法函数零值测试
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, multiply(0, 0))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "greet_function_special_chars" {
  // 问候函数特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let a = 10
  let b = 5
  let c = multiply(a, b)  // 50
  let d = add(c, 20)       // 70
  let e = divide_with_ceil(d, 8)  // 9
  assert_eq(9, e)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "error_handling_division" {
  // 除法错误处理测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "unicode_comprehensive_support" {
  // 全面Unicode支持测试
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
}