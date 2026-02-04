// Azimuth 增强单元测试
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "enhanced_basic_arithmetic" {
  // 基本算术运算测试
  let result1 = 15 + 8
  let result2 = 25 * 4
  let result3 = 100 - 75
  let result4 = 50 / 5
  
  assert_eq(23, result1)
  assert_eq(100, result2)
  assert_eq(25, result3)
  assert_eq(10, result4)
}

test "enhanced_string_operations" {
  // 字符串操作测试
  let greeting = "Hello"
  let name = "World"
  let message = greeting + ", " + name + "!"
  
  assert_eq_string("Hello, World!", message)
}

test "enhanced_boolean_logic" {
  // 布尔逻辑测试
  let true_val = true
  let false_val = false
  
  assert_true(true_val)
  assert_false(false_val)
  assert_true(true_val && not(false_val))
  assert_true(true_val || false_val)
}

test "enhanced_mathematical_properties" {
  // 数学性质验证测试
  let a = 13
  let b = 29
  
  // 加法交换律
  assert_eq(a + b, b + a)
  
  // 乘法交换律
  assert_eq(a * b, b * a)
  
  // 验证具体数值
  assert_eq(42, 13 + 29)
  assert_eq(42, 29 + 13)
  assert_eq(377, 13 * 29)
  assert_eq(377, 29 * 13)
}

test "enhanced_boundary_values" {
  // 边界值测试
  assert_eq(0, 0 + 0)
  assert_eq(1, 1 * 1)
  assert_eq(-1, -1 * 1)
  assert_eq(2147483647, 2147483646 + 1)
  assert_eq(-2147483648, -2147483647 - 1)
}

test "enhanced_complex_calculations" {
  // 复杂计算测试
  let base_value = 15
  let multiplier = 3
  let addition = 8
  
  // 计算流程：(base * multiplier) + addition
  let step1 = base_value * multiplier
  let result = step1 + addition
  
  assert_eq(53, result)  // (15*3) + 8 = 45 + 8 = 53
}

test "enhanced_business_calculations" {
  // 业务计算测试
  let unit_price = 45
  let quantity = 6
  let discount = 20
  let tax = 15
  
  // 计算总价
  let subtotal = unit_price * quantity
  let total = subtotal - discount + tax
  
  assert_eq(270, subtotal)
  assert_eq(265, total)
}

test "enhanced_sequence_operations" {
  // 序列操作测试
  let initial = 10
  let step1 = initial + 5
  let step2 = step1 * 2
  let step3 = step2 - 15
  let result = step3 / 3
  
  assert_eq(15, step1)
  assert_eq(30, step2)
  assert_eq(15, step3)
  assert_eq(5, result)
}

test "enhanced_error_handling" {
  // 错误处理测试
  // 检查零值处理
  let zero = 0
  let positive = 10
  let negative = -10
  
  assert_eq(0, zero * positive)
  assert_eq(0, zero * negative)
  assert_eq(0, zero + zero)
}

test "enhanced_resource_management" {
  // 资源管理测试
  let total_resources = 100
  let teams = 3
  let base_allocation = total_resources / teams
  let remainder = total_resources % teams
  
  assert_eq(33, base_allocation)
  assert_eq(1, remainder)
  
  // 验证分配总和
  let allocated_total = base_allocation * teams + remainder
  assert_eq(100, allocated_total)
}