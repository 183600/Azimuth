// 新增的10个标准MoonBit测试用例
test "basic_arithmetic_comprehensive" {
  // 基础算术综合测试
  assert_eq(15, add(8, 7))
  assert_eq(-1, add(5, -6))
  assert_eq(0, add(0, 0))
  assert_eq(100, multiply(10, 10))
  assert_eq(-100, multiply(10, -10))
  assert_eq(0, multiply(0, 100))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(5, divide_with_ceil(9, 2))
  assert_eq(10, divide_with_ceil(19, 2))
  assert_eq(100, divide_with_ceil(199, 2))
}

test "string_greeting_variations" {
  // 字符串问候变化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, spaces!", greet("spaces"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "mathematical_commutativity" {
  // 数学交换律测试
  let a = 17
  let b = 23
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(40, add(a, b))
  assert_eq(391, multiply(a, b))
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let workers = 15
  let tasks_per_worker = 8
  let total_tasks = multiply(workers, tasks_per_worker)
  let days_needed = divide_with_ceil(total_tasks, 50)
  
  assert_eq(120, total_tasks)
  assert_eq(3, days_needed)
  
  // 验证总工作量
  let daily_capacity = 50
  let actual_capacity = multiply(days_needed, daily_capacity)
  assert_eq(150, actual_capacity)
}

test "negative_value_operations" {
  // 负值运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(10, add(-5, 15))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 最小值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 单位值测试
  assert_eq(1, add(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 除零安全测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let initial_value = 20
  let factor1 = 3
  let factor2 = 2
  let divisor = 5
  let offset = 7
  
  // 计算流程：((initial * factor1) + (initial * factor2)) / divisor + offset
  let step1 = multiply(initial_value, factor1)
  let step2 = multiply(initial_value, factor2)
  let step3 = add(step1, step2)
  let step4 = divide_with_ceil(step3, divisor)
  let result = add(step4, offset)
  
  assert_eq(19, result)  // ceil((20*3 + 20*2)/5) + 7 = ceil(100/5) + 7 = 20 + 7 = 27
}

test "inventory_management_comprehensive" {
  // 库存管理综合测试
  let product_a = 45
  let product_b = 67
  let product_c = 23
  let box_capacity = 20
  
  // 计算各产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  assert_eq(3, boxes_a)  // ceil(45/20) = 3
  assert_eq(4, boxes_b)  // ceil(67/20) = 4
  assert_eq(2, boxes_c)  // ceil(23/20) = 2
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(9, total_boxes)
  assert_eq(180, total_capacity)
  
  // 计算总产品数
  let total_products = add(add(product_a, product_b), product_c)
  assert_eq(135, total_products)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 各种除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  
  // 字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))
}