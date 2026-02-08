// Azimuth 项目基础测试套件
// 包含 8 个 MoonBit 测试用例
// 使用内置操作符和基本验证方法

test "basic_arithmetic_addition" {
  // 基本加法测试
  let result1 = 2 + 3
  let result2 = 10 + 15
  let result3 = -5 + 8
  
  if (result1 != 5) {
    @builtin.abort("Test failed: 2 + 3 should be 5")
  }
  if (result2 != 25) {
    @builtin.abort("Test failed: 10 + 15 should be 25")
  }
  if (result3 != 3) {
    @builtin.abort("Test failed: -5 + 8 should be 3")
  }
}

test "basic_arithmetic_multiplication" {
  // 基本乘法测试
  let result1 = 4 * 6
  let result2 = 7 * 8
  let result3 = -3 * 5
  
  if (result1 != 24) {
    @builtin.abort("Test failed: 4 * 6 should be 24")
  }
  if (result2 != 56) {
    @builtin.abort("Test failed: 7 * 8 should be 56")
  }
  if (result3 != -15) {
    @builtin.abort("Test failed: -3 * 5 should be -15")
  }
}

test "basic_arithmetic_subtraction" {
  // 基本减法测试
  let result1 = 10 - 3
  let result2 = 15 - 20
  let result3 = 8 - 8
  
  if (result1 != 7) {
    @builtin.abort("Test failed: 10 - 3 should be 7")
  }
  if (result2 != -5) {
    @builtin.abort("Test failed: 15 - 20 should be -5")
  }
  if (result3 != 0) {
    @builtin.abort("Test failed: 8 - 8 should be 0")
  }
}

test "basic_arithmetic_division" {
  // 基本除法测试
  let result1 = 20 / 4
  let result2 = 15 / 3
  let result3 = 7 / 2
  
  if (result1 != 5) {
    @builtin.abort("Test failed: 20 / 4 should be 5")
  }
  if (result2 != 5) {
    @builtin.abort("Test failed: 15 / 3 should be 5")
  }
  if (result3 != 3) {
    @builtin.abort("Test failed: 7 / 2 should be 3")
  }
}

test "complex_calculations" {
  // 复合计算测试
  let result1 = (2 + 3) * 4
  let result2 = 10 * 2 + 5
  let result3 = (15 - 5) / 2
  
  if (result1 != 20) {
    @builtin.abort("Test failed: (2 + 3) * 4 should be 20")
  }
  if (result2 != 25) {
    @builtin.abort("Test failed: 10 * 2 + 5 should be 25")
  }
  if (result3 != 5) {
    @builtin.abort("Test failed: (15 - 5) / 2 should be 5")
  }
}

test "financial_calculation" {
  // 金融计算测试
  let principal = 10000
  let rate = 5
  let interest = principal * rate / 100
  let total = principal + interest
  
  if (interest != 500) {
    @builtin.abort("Test failed: interest should be 500")
  }
  if (total != 10500) {
    @builtin.abort("Test failed: total should be 10500")
  }
}

test "inventory_calculation" {
  // 库存计算测试
  let items_per_box = 24
  let total_items = 100
  let boxes_needed = (total_items + items_per_box - 1) / items_per_box  // 向上取整
  
  let total_capacity = boxes_needed * items_per_box
  let empty_space = total_capacity - total_items
  
  if (boxes_needed != 5) {
    @builtin.abort("Test failed: boxes_needed should be 5")
  }
  if (total_capacity != 120) {
    @builtin.abort("Test failed: total_capacity should be 120")
  }
  if (empty_space != 20) {
    @builtin.abort("Test failed: empty_space should be 20")
  }
}

test "percentage_calculation" {
  // 百分比计算测试
  let total = 200
  let percentage = 25
  let result = total * percentage / 100
  
  let increased_total = total + result
  let decreased_total = total - result
  
  if (result != 50) {
    @builtin.abort("Test failed: result should be 50")
  }
  if (increased_total != 250) {
    @builtin.abort("Test failed: increased_total should be 250")
  }
  if (decreased_total != 150) {
    @builtin.abort("Test failed: decreased_total should be 150")
  }
}