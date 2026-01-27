// 标准 MoonBit 测试用例 - 为 azimuth 库创建的10个高质量测试
// 测试覆盖基本数学运算、字符串处理和业务逻辑场景

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-5, 15))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "subtract_function_basic_cases" {
  // 测试减法函数的基本情况（使用 add 函数的负数形式模拟）
  assert_eq(2, add(5, -3))
  assert_eq(-2, add(3, -5))
  assert_eq(0, add(5, -5))
  assert_eq(10, add(5, 5))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = add(total_capacity, -total_items)
  assert_eq(5, remaining_space)
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial = 10
  let step1 = add(initial, multiply(2, 3))     // 10 + 2*3 = 16
  let step2 = divide_with_ceil(step1, 4)       // ceil(16/4) = 4
  let step3 = multiply(step2, add(1, 2))       // 4 * (1+2) = 12
  let step4 = divide_with_ceil(step3, 5)       // ceil(12/5) = 3
  
  assert_eq(3, step4)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-5, -5))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "resource_allocation_scenario" {
  // 测试资源分配场景
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, resources_per_team)
  
  // 验证分配结果
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(102, total_allocated)
  
  // 验证资源利用率
  assert_true(total_allocated >= total_resources)
}