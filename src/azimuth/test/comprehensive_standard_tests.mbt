// 综合标准测试用例
// 覆盖 azimuth 包的核心功能和边界情况

// 1. divide_with_ceil 函数全面测试
test "divide_with_ceil_comprehensive" {
  // 基本功能测试
  assert_eq(2, divide_with_ceil(10, 5))
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(4, divide_with_ceil(10, 3))
  
  // 边界情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  
  // 零值测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
}

// 2. divide_with_ceil 负数测试
test "divide_with_ceil_negative_numbers" {
  // 负数被除数测试
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-2, divide_with_ceil(-11, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  
  // 混合符号测试
  assert_eq(-1, divide_with_ceil(-5, 10))
  assert_eq(-1, divide_with_ceil(-1, 100))
  assert_eq(-1, divide_with_ceil(-100, 100))
}

// 3. greet 函数 Unicode 和特殊字符测试
test "greet_function_unicode_extended" {
  // Unicode 字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, Ñoño!", greet("Ñoño"))
  
  // 特殊空白字符测试
  assert_eq_string("Hello, 	Tab!", greet("\tTab"))
  assert_eq_string("Hello, \nNewLine!", greet("\nNewLine"))
  
  // 长字符串测试
  let long_name = "这是一个非常长的名字用来测试greet函数处理长字符串的能力"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

// 4. 数学性质综合验证测试
test "mathematical_properties_comprehensive" {
  // 加法性质验证
  let a = 123
  let b = 456
  let c = 789
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法性质验证
  let x = 12
  let y = 34
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  let z = 5
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  
  // 零元素性质
  assert_eq(0, multiply(0, x))
  assert_eq(x, multiply(x, 1))
}

// 5. 实际应用场景：项目管理测试
test "project_management_scenario" {
  // 项目任务分配场景
  let total_tasks = 47
  let team_members = 5
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  // 验证任务分配
  assert_true(tasks_per_member >= total_tasks / team_members)
  assert_true(multiply(tasks_per_member, team_members) >= total_tasks)
  
  // 计算项目总工作量
  let avg_task_hours = 8
  let buffer_hours = 10
  let total_hours = add(multiply(tasks_per_member, avg_task_hours), buffer_hours)
  
  assert_true(total_hours > 0)
}

// 6. 边界值组合测试
test "boundary_values_combination" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值边界测试
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 边界值运算测试
  assert_eq(max_val, add(max_val - 1, 1))
  assert_eq(min_val, add(min_val + 1, -1))
}

// 7. 字符串处理边界测试
test "string_processing_edge_cases" {
  // 空字符串和单字符测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  
  // 特殊组合字符测试
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, [bracketed]!", greet("[bracketed]"))
  
  // 数字字符串测试
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 3.14159!", greet("3.14159"))
}

// 8. 复杂数学计算测试
test "complex_mathematical_calculation" {
  // 模拟计算圆的面积和周长
  let radius = 7
  let pi_approx = 22  // 使用 22/7 作为 pi 的近似值
  let diameter = multiply(radius, 2)
  
  // 计算周长 (近似)
  let circumference = multiply(diameter, pi_approx) / 7
  assert_eq(44, circumference)
  
  // 计算面积 (近似)
  let area = multiply(radius, multiply(radius, pi_approx)) / 7
  assert_eq(154, area)
  
  // 验证几何关系
  assert_true(circumference > diameter)
  assert_true(area > radius)
}