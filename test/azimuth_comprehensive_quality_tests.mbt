// Azimuth 综合质量测试用例 - 8个高质量标准 MoonBit 测试
// 覆盖核心功能、边界情况和实际应用场景

test "add_function_commutative_property" {
  // 测试加法交换律性质
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律性质
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证具体数值: 6 * (4 + 3) = 6 * 4 + 6 * 3 = 42
  assert_eq(42, left_side)
  assert_eq(42, right_side)
}

test "subtract_function_negative_handling" {
  // 测试减法函数的负数处理
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(5, -10))
  assert_eq(-15, subtract(-5, 10))
  assert_eq(5, subtract(-5, -10))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  // 场景1: 计算会议室需求
  let attendees = 47
  let capacity_per_room = 10
  let rooms_needed = divide_with_ceil(attendees, capacity_per_room)
  assert_eq(5, rooms_needed)
  
  // 场景2: 计算运输批次
  let packages = 156
  let packages_per_truck = 25
  let trucks_needed = divide_with_ceil(packages, packages_per_truck)
  assert_eq(7, trucks_needed)
  
  // 场景3: 零除数处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
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

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景: 项目预算分配
  let total_budget = 50000
  let departments = 7
  let base_budget_per_dept = divide_with_ceil(total_budget, departments)
  assert_eq(7143, base_budget_per_dept)  // ceil(50000/7)
  
  // 计算实际分配总额
  let actual_allocation = multiply(base_budget_per_dept, departments)
  assert_eq(50001, actual_allocation)
  
  // 计算预算溢出
  let budget_overflow = subtract(actual_allocation, total_budget)
  assert_eq(1, budget_overflow)
  
  // 验证每个部门都能获得足够预算
  assert_true(base_budget_per_dept >= divide_with_ceil(total_budget, departments))
}

test "mathematical_sequence_operations" {
  // 测试数学序列操作
  // 斐波那契式计算
  let a = 1
  let b = 1
  
  let fib_3 = add(a, b)  // 2
  let fib_4 = add(b, fib_3)  // 3
  let fib_5 = add(fib_3, fib_4)  // 5
  
  assert_eq(2, fib_3)
  assert_eq(3, fib_4)
  assert_eq(5, fib_5)
  
  // 等差数列求和
  let first_term = 3
  let common_diff = 2
  let num_terms = 5
  
  let last_term = add(first_term, multiply(common_diff, subtract(num_terms, 1)))
  let sum = multiply(divide_with_ceil(num_terms, 2), add(first_term, last_term))
  
  assert_eq(11, last_term)  // 3 + 2*(5-1) = 11
  assert_eq(35, sum)  // 5/2 * (3+11) = 35
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边缘情况
  // 极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, subtract(0, 0))
  
  // 大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  
  // 字符串边缘情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
}