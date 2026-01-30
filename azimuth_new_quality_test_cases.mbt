// Azimuth 新增质量测试用例
// 包含10个高质量的 MoonBit 标准测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  add(3, 4) = 7
  add(-2, 5) = 3
  add(0, 0) = 0
  
  multiply(3, 4) = 12
  multiply(-2, 5) = -10
  multiply(0, 5) = 0
  
  subtract(10, 3) = 7
  subtract(5, 10) = -5
  subtract(0, 0) = 0
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(1, 1) = 1
  divide_with_ceil(0, 5) = 0
  divide_with_ceil(15, 0) = 0
  
  // 负数测试
  divide_with_ceil(-10, 3) = -3
  divide_with_ceil(10, -3) = -4
  divide_with_ceil(-10, -3) = 3
}

test "greet_function_tests" {
  // 问候函数测试
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
  greet("") = "Hello, !"
}

test "mathematical_properties" {
  // 数学性质验证
  let x = 8
  let y = 6
  
  // 加法交换律
  add(x, y) = add(y, x)
  
  // 乘法交换律
  multiply(x, y) = multiply(y, x)
  
  // 分配律
  multiply(x, add(y, 2)) = add(multiply(x, y), multiply(x, 2))
}

test "business_logistics_scenario" {
  // 物流业务场景测试
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  
  trucks_needed = 5
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  total_capacity = 50
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, packages)
  remaining_space = 3
}

test "project_resource_planning" {
  // 项目资源规划测试
  let team_members = 5
  let work_days = 10
  let daily_hours = 8
  let task_hours = 400
  
  // 计算总工时
  let total_available_hours = multiply(team_members, multiply(work_days, daily_hours))
  total_available_hours = 400
  
  // 计算需要的额外天数
  let additional_days_needed = divide_with_ceil(task_hours, multiply(team_members, daily_hours))
  additional_days_needed = 10
}

test "boundary_values_test" {
  // 边界值测试
  // 零值测试
  add(0, 0) = 0
  multiply(5, 0) = 0
  divide_with_ceil(0, 10) = 0
  divide_with_ceil(0, 0) = 0
  
  // 单位值测试
  add(5, 0) = 5
  multiply(5, 1) = 5
  divide_with_ceil(5, 5) = 1
  
  // 负数边界测试
  add(-5, -5) = -10
  multiply(-5, -5) = 25
  divide_with_ceil(-10, 5) = -2
}

test "string_processing_tests" {
  // 字符串处理测试
  greet("single") = "Hello, single!"
  greet("12345") = "Hello, 12345!"
  greet("@#$%") = "Hello, @#$%!"
  greet("mixedCASE") = "Hello, mixedCASE!"
  greet("  ") = "Hello,   !"
}

test "financial_calculation_test" {
  // 金融计算测试
  let loan_amount = 10000
  let monthly_payment_capacity = 800
  let months_needed = divide_with_ceil(loan_amount, monthly_payment_capacity)
  
  months_needed = 13
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment_capacity)
  total_payment = 10400
  
  // 验证最后一个月的实际支付
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment_capacity))
  final_payment = 800
}

test "warehouse_optimization_test" {
  // 仓库优化测试
  let item_a_quantity = 35
  let item_b_quantity = 47
  let item_c_quantity = 18
  let box_capacity = 15
  
  // 计算各物品需要的箱子数
  let boxes_a = divide_with_ceil(item_a_quantity, box_capacity)
  let boxes_b = divide_with_ceil(item_b_quantity, box_capacity)
  let boxes_c = divide_with_ceil(item_c_quantity, box_capacity)
  
  boxes_a = 3  // ceil(35/15) = 3
  boxes_b = 4  // ceil(47/15) = 4
  boxes_c = 2  // ceil(18/15) = 2
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  total_boxes = 9
  total_capacity = 135
  
  // 验证总物品数量
  let total_items = add(add(item_a_quantity, item_b_quantity), item_c_quantity)
  total_items = 100
}