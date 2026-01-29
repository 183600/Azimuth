// 标准 MoonBit 测试套件 - 新增10个高质量测试用例

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(5, multiply(1, 5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_function_basic" {
  // 测试减法函数基本功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(8, subtract(5, -3))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 3
  let b = 4
  let c = 5
  
  // a * (b + c) = a * b + a * c
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let items = 23
  let capacity_per_box = 5
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(25, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(2, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base = 10
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算流程：(base * multiplier) / divisor + addition
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(14, result)  // ceil(10*3/4) + 7 = 8 + 7 = 15
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(2, divide_with_ceil(-7, -3))
}