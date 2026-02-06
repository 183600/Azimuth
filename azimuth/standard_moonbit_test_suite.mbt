// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，涵盖各种核心功能

// 测试用例1: 基本数学运算
test "basic_mathematical_operations" {
  // 测试加法
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  
  // 测试乘法
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-12, multiply(-3, 4))
  
  // 测试减法
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
}

// 测试用例2: 向上取整除法的各种情况
test "divide_with_ceil_comprehensive" {
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

// 测试用例3: 字符串处理和问候功能
test "string_processing_and_greeting" {
  // 基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 特殊输入情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello,   !", greet("  "))
  
  // Unicode和特殊字符
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, éspanol!", greet("éspanol"))
}

// 测试用例4: 数学性质和恒等式
test "mathematical_properties_and_identities" {
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(a, a))
  assert_eq(a, subtract(a, 0))
}

// 测试用例5: 业务场景计算
test "business_scenario_calculations" {
  // 库存管理场景
  let total_products = 47
  let package_capacity = 12
  let packages_needed = divide_with_ceil(total_products, package_capacity)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
  
  // 项目管理场景
  let team_members = 6
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(42, total_tasks)
  assert_eq(3, sprints_needed)
}

// 测试用例6: 边界值和极值处理
test "boundary_values_and_extremes" {
  // 零值处理
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  
  // 单位值处理
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

// 测试用例7: 复杂数学运算链
test "complex_mathematical_operations_chain" {
  // 连续运算测试
  let initial = 5
  
  // 运算链: ((5 + 3) * 2) / 4 = 4
  let step1 = add(initial, 3)        // 8
  let step2 = multiply(step1, 2)     // 16
  let step3 = divide_with_ceil(step2, 4)  // 4
  assert_eq(4, step3)
  
  // 更复杂的运算链
  let a = 10
  let b = 3
  let c = 7
  
  // ((a * b) + (a * c)) / (b + c) = a
  let numerator = add(multiply(a, b), multiply(a, c))
  let denominator = add(b, c)
  let result = divide_with_ceil(numerator, denominator)
  assert_eq(10, result)
}

// 测试用例8: 错误处理和异常情况
test "error_handling_and_exception_cases" {
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试负数乘法（使用较小值避免溢出）
  let negative_product = multiply(-1000, 1000)
  assert_eq(-1000000, negative_product)
  
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
}

// 测试用例9: 数学分配律测试
test "mathematical_distributive_property" {
  let a = 6
  let b = 4
  let c = 3
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

// 测试用例10: 连续计算链测试
test "sequential_calculation_chain" {
  let initial = 5
  
  // 连续计算: ((5 + 3) * 2) / 4 + 1 = 5
  let step1 = add(initial, 3)           // 8
  let step2 = multiply(step1, 2)        // 16
  let step3 = divide_with_ceil(step2, 4) // 4
  let step4 = add(step3, 1)             // 5
  assert_eq(5, step4)
}