// 标准 MoonBit 测试用例
// 为 azimuth 项目添加的高质量测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(3, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "ceil_division_basic" {
  // 向上取整除法基本测试
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(3, divide_with_ceil(6, 2))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-1, divide_with_ceil(-5, 3))
}

test "mathematical_commutativity" {
  // 数学交换律测试
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "subtraction_functionality" {
  // 减法功能测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(5, -10))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 10
  let step1 = multiply(base, 3)  // 30
  let step2 = add(step1, 15)     // 45
  let step3 = divide_with_ceil(step2, 8)  // 6
  
  assert_eq(6, step3)
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "edge_case_handling" {
  // 边界情况处理测试
  // 零值运算
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  
  // 单位运算
  assert_eq(5, multiply(5, 1))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 25
  let quantity = 8
  let discount_threshold = 100
  let discount_rate = 10
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal >= discount_threshold) {
    divide_with_ceil(multiply(subtotal, discount_rate), 100)
  } else {
    0
  }
  
  // 计算总计
  let total = subtract(subtotal, discount)
  
  assert_eq(200, subtotal)
  assert_eq(20, discount)
  assert_eq(180, total)
}