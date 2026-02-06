// 简单的 MoonBit 测试套件
// 为 Azimuth 项目添加的基本测试用例

test "simple_add_test" {
  // 简单加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "simple_multiply_test" {
  // 简单乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greet_test" {
  // 简单问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_divide_test" {
  // 简单除法测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_subtract_test" {
  // 简单减法测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "simple_math_properties" {
  // 简单数学性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "simple_business_scenario" {
  // 简单业务场景测试
  let items = 25
  let capacity = 6
  let boxes = divide_with_ceil(items, capacity)
  
  assert_eq(5, boxes)
  
  // 验证总容量
  let total_capacity = multiply(boxes, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)
}

test "simple_edge_cases" {
  // 简单边界情况测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(10000, multiply(100, 100))
}

test "simple_complex_workflow" {
  // 简单复杂工作流测试
  let initial = 10
  let step1 = add(initial, multiply(2, 3))  // 10 + 2*3 = 16
  let step2 = divide_with_ceil(step1, 5)    // ceil(16/5) = 4
  let final_result = add(step2, 1)          // 4 + 1 = 5
  
  assert_eq(5, final_result)
}

test "simple_unicode_test" {
  // 简单Unicode测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}