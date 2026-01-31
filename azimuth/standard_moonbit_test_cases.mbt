// 标准 MoonBit 测试用例 - 覆盖 Azimuth 项目核心功能
// 包含 10 个高质量测试用例，遵循 MoonBit 测试语法规范

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(10, add(-5, 15))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(12, multiply(-3, -4))
}

test "greet_function_basic_cases" {
  // 测试问候函数的基本情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(1, -3))
}

test "subtract_function_basic_cases" {
  // 测试基本减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "mathematical_identity_elements" {
  // 测试数学单位元性质
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(0, multiply(5, 0))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let step1 = add(3, 4)
  let step2 = multiply(step1, 2)
  let step3 = divide_with_ceil(step2, 3)
  let result = add(step3, 1)
  assert_eq(5, result)  // ((3+4)*2)/3 + 1 = 14/3 + 1 = 5 + 1 = 5
}

test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

// 新增的10个标准 MoonBit 测试用例
test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(1000000, subtract(1000000, 0))
}

test "error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "string_processing_edge_cases" {
  // 测试字符串处理边缘情况
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "financial_calculation_scenario" {
  // 金融计算场景：贷款分期计算
  let loan_amount = 10000
  let monthly_payment = 3500
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(3, months_needed)  // ceil(10000/3500) = 3
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(10500, total_payment)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  let total_products = 250
  let box_capacity = 30
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(270, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(20, remaining_space)
}

test "performance_large_scale_operations" {
  // 大规模运算性能测试
  let iterations = 10000
  let sum_result = add(iterations, iterations)
  let product_result = multiply(100, iterations)
  let division_result = divide_with_ceil(product_result, 50)
  
  assert_eq(20000, sum_result)
  assert_eq(1000000, product_result)
  assert_eq(20000, division_result)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "boundary_values_comprehensive" {
  // 边界值综合测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 验证总座位数正确
  assert_eq(88, total_seats)
}