// Azimuth 库的补充测试用例
// 提供额外的测试覆盖，确保代码质量和功能完整性

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(100, subtract(50, -50))
}

test "mathematical_identity_properties_extended" {
  // 扩展的数学恒等式测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
  assert_eq(-x, subtract(0, x))
}

test "divide_with_ceil_precision_edge_cases" {
  // 向上取整除法的精度边缘情况测试
  // 测试接近整数边界的情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  
  // 测试大数精度
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(667, divide_with_ceil(2000, 3))
  assert_eq(334, divide_with_ceil(1000, 3))
  
  // 测试小结果
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(1, divide_with_ceil(1, 999))
}

test "greet_function_unicode_comprehensive" {
  // 问候函数的Unicode综合测试
  assert_eq_string("Hello, 🌙⭐🌟!", greet("🌙⭐🌟"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिर्दी!", greet("हिर्दी"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, ελληνικά!", greet("ελληνικά"))
}

test "complex_inventory_management" {
  // 复杂库存管理场景测试
  // 场景：多产品仓库管理
  
  // 产品A：157件，每箱24件
  let product_a_qty = 157
  let product_a_capacity = 24
  let boxes_a = divide_with_ceil(product_a_qty, product_a_capacity)
  
  // 产品B：89件，每箱16件
  let product_b_qty = 89
  let product_b_capacity = 16
  let boxes_b = divide_with_ceil(product_b_qty, product_b_capacity)
  
  // 产品C：23件，每箱8件
  let product_c_qty = 23
  let product_c_capacity = 8
  let boxes_c = divide_with_ceil(product_c_qty, product_c_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  // 验证结果
  assert_eq(7, boxes_a)  // ceil(157/24) = 7
  assert_eq(6, boxes_b)  // ceil(89/16) = 6
  assert_eq(3, boxes_c)  // ceil(23/8) = 3
  assert_eq(16, total_boxes)  // 7 + 6 + 3 = 16
  
  // 计算总存储容量
  let total_capacity = add(
    multiply(boxes_a, product_a_capacity),
    add(
      multiply(boxes_b, product_b_capacity),
      multiply(boxes_c, product_c_capacity)
    )
  )
  assert_eq(304, total_capacity)  // 7*24 + 6*16 + 3*8 = 168 + 96 + 24 = 288
}

test "mathematical_operation_precedence" {
  // 数学运算优先级测试
  let a = 10
  let b = 5
  let c = 2
  
  // 测试运算优先级和括号的影响
  let result1 = add(multiply(a, b), c)  // (10 * 5) + 2 = 52
  let result2 = multiply(a, add(b, c))  // 10 * (5 + 2) = 70
  let result3 = divide_with_ceil(subtract(a, b), c)  // ceil((10 - 5) / 2) = 3
  let result4 = subtract(divide_with_ceil(a, b), c)  // ceil(10 / 5) - 2 = 0
  
  assert_eq(52, result1)
  assert_eq(70, result2)
  assert_eq(3, result3)
  assert_eq(0, result4)
}

test "error_boundary_comprehensive" {
  // 综合错误边界测试
  // 除零情况的全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试加法极值
  assert_eq(max_int - 1, add(max_int - 2, 1))
  assert_eq(min_int + 1, add(min_int + 2, -1))
  
  // 测试乘法极值（避免溢出）
  assert_eq(46340, multiply(46340, 1))  // sqrt(2^31-1) ≈ 46340
  assert_eq(-46340, multiply(-46340, 1))
}

test "real_world_project_management" {
  // 真实世界项目管理场景测试
  // 场景：软件开发项目的资源规划
  
  let team_members = 8
  let sprint_weeks = 2
  let hours_per_week = 40
  let story_points_per_hour = 2
  let total_story_points = 320
  
  // 计算团队总工时
  let total_team_hours = multiply(
    multiply(team_members, hours_per_week),
    sprint_weeks
  )
  
  // 计算总故事点处理能力
  let total_story_capacity = multiply(total_team_hours, story_points_per_hour)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(
    total_story_points,
    total_story_capacity
  )
  
  // 验证结果
  assert_eq(640, total_team_hours)  // 8 * 40 * 2 = 640
  assert_eq(1280, total_story_capacity)  // 640 * 2 = 1280
  assert_eq(1, sprints_needed)  // ceil(320/1280) = 1
  
  // 计算资源利用率
  let utilization = divide_with_ceil(
    multiply(total_story_points, 100),
    total_story_capacity
  )
  assert_eq(25, utilization)  // ceil(32000/1280) = 25%
}

test "string_formatting_special_cases" {
  // 字符串格式化特殊案例测试
  // 测试各种特殊字符串和格式
  
  // 空白字符测试
  assert_eq_string("Hello, 	
\r!", greet("	
\r"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \u0020!", greet("\u0020"))  // Unicode空格
  
  // 特殊符号测试
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greet("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  
  // 数字和字母混合
  assert_eq_string("Hello, test123ABC!", greet("test123ABC"))
  assert_eq_string("Hello, 123test456!", greet("123test456"))
  
  // 引号和转义字符
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
}