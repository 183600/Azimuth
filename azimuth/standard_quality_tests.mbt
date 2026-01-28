// 标准 MoonBit 测试用例 - 高质量测试集
// 覆盖 azimuth 库的核心功能和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律性质
  assert_eq(add(15, 27), add(27, 15))
  assert_eq(add(-10, 25), add(25, -10))
  assert_eq(add(0, 100), add(100, 0))
  assert_eq(add(-50, -75), add(-75, -50))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律性质
  let a = 6
  let b = 8
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // (a + b) * c = a * c + b * c
  let left_side2 = multiply(add(a, b), c)
  let right_side2 = add(multiply(a, c), multiply(b, c))
  assert_eq(left_side2, right_side2)
}

test "divide_with_ceil_real_world_scenarios" {
  // 测试向上取整除法的真实世界场景
  
  // 场景1：计算需要的运输车辆
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 场景2：计算项目完成需要的迭代次数
  let total_tasks = 38
  let tasks_per_iteration = 8
  let iterations_needed = divide_with_ceil(total_tasks, tasks_per_iteration)
  assert_eq(5, iterations_needed)
  
  // 场景3：计算需要的存储空间
  let data_size = 1025
  let block_size = 256
  let blocks_needed = divide_with_ceil(data_size, block_size)
  assert_eq(5, blocks_needed)
}

test "subtract_function_boundary_conditions" {
  // 测试减法函数的边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正常减法
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  
  // 负数减法
  assert_eq(-10, subtract(-5, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(0, subtract(-10, -10))
  
  // 边界值减法
  assert_eq(max_val, subtract(max_val, 0))
  assert_eq(min_val, subtract(min_val, 0))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算订单总成本（包括税费和折扣）
  
  let base_price = 100
  let quantity = 7
  let discount_rate = 5  // 5% discount
  let tax_rate = 8      // 8% tax
  
  // 计算小计
  let subtotal = multiply(base_price, quantity)
  assert_eq(700, subtotal)
  
  // 计算折扣
  let discount_amount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
  assert_eq(35, discount_amount)
  
  // 计算折后价格
  let discounted_price = subtract(subtotal, discount_amount)
  assert_eq(665, discounted_price)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(discounted_price, tax_rate), 100)
  assert_eq(54, tax_amount)
  
  // 计算总价
  let total_price = add(discounted_price, tax_amount)
  assert_eq(719, total_price)
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  
  // 加法恒等律：a + 0 = a
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(42, add(42, 0))
  assert_eq(-42, add(-42, 0))
  
  // 乘法恒等律：a * 1 = a
  assert_eq(0, multiply(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(-42, 1))
  
  // 乘法零律：a * 0 = 0
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, multiply(-1, 0))
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-42, 0))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化算法
  // 场景：将资源分配给不同项目，确保每个项目获得最小所需资源
  
  let total_resources = 100
  let project1_min = 25
  let project2_min = 30
  let project3_min = 20
  let project4_min = 15
  
  // 计算最小总需求
  let min_total_required = add(add(project1_min, project2_min), add(project3_min, project4_min))
  assert_eq(90, min_total_required)
  
  // 计算剩余资源
  let remaining_resources = subtract(total_resources, min_total_required)
  assert_eq(10, remaining_resources)
  
  // 平均分配剩余资源
  let projects_count = 4
  let bonus_per_project = divide_with_ceil(remaining_resources, projects_count)
  assert_eq(3, bonus_per_project)
  
  // 计算每个项目的最终分配
  let project1_final = add(project1_min, bonus_per_project)
  let project2_final = add(project2_min, bonus_per_project)
  let project3_final = add(project3_min, bonus_per_project)
  let project4_final = add(project4_min, bonus_per_project)
  
  assert_eq(28, project1_final)
  assert_eq(33, project2_final)
  assert_eq(23, project3_final)
  assert_eq(18, project4_final)
  
  // 验证总分配
  let total_allocated = add(add(project1_final, project2_final), add(project3_final, project4_final))
  assert_eq(102, total_allocated)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  
  // 极值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(" "))
  assert_eq_string("Hello, x!", greet("x"))
}