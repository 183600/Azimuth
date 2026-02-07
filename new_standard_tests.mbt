// 新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "power_calculation_simulation" {
  // 幂运算模拟测试
  // 计算 2^5 = 32
  let base = 2
  let exponent = 5
  
  let power1 = base                    // 2^1 = 2
  let power2 = multiply(power1, base)  // 2^2 = 4
  let power3 = multiply(power2, base)  // 2^3 = 8
  let power4 = multiply(power3, base)  // 2^4 = 16
  let power5 = multiply(power4, base)  // 2^5 = 32
  
  assert_eq(2, power1)
  assert_eq(4, power2)
  assert_eq(8, power3)
  assert_eq(16, power4)
  assert_eq(32, power5)
}

test "greatest_common_divisor" {
  // 最大公约数计算测试
  // 使用欧几里得算法模拟计算 GCD(48, 18) = 6
  
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法步骤
  let step1_remainder = a % b        // 48 % 18 = 12
  let step2_remainder = b % step1_remainder  // 18 % 12 = 6
  let step3_remainder = step1_remainder % step2_remainder  // 12 % 6 = 0
  
  // GCD 是最后一个非零余数
  let gcd = step2_remainder
  
  assert_eq(12, step1_remainder)
  assert_eq(6, step2_remainder)
  assert_eq(0, step3_remainder)
  assert_eq(6, gcd)
}

test "average_calculation" {
  // 平均值计算测试
  let numbers_sum = add(add(add(add(15, 25), 35), 45), 55)
  let count = 5
  let average = divide_with_ceil(numbers_sum, count)
  
  assert_eq(175, numbers_sum)
  assert_eq(35, average)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let part = 50
  let percentage = divide_with_ceil(multiply(part, 100), total)
  
  // 计算 25% 增长
  let original = 80
  let growth_rate = 25
  let increase = divide_with_ceil(multiply(original, growth_rate), 100)
  let new_total = add(original, increase)
  
  assert_eq(25, percentage)
  assert_eq(20, increase)
  assert_eq(100, new_total)
}

test "time_calculations" {
  // 时间计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(multiply(hours, 3600), add(multiply(minutes, 60), seconds))
  
  // 计算总分钟数（向上取整）
  let total_minutes = divide_with_ceil(total_seconds, 60)
  
  // 计算总小时数（向上取整）
  let total_hours = divide_with_ceil(total_seconds, 3600)
  
  assert_eq(9930, total_seconds)
  assert_eq(166, total_minutes)
  assert_eq(3, total_hours)
}

test "array_sum_simulation" {
  // 数组求和模拟测试
  // 模拟数组 [10, 20, 30, 40, 50] 的求和过程
  
  let element1 = 10
  let element2 = 20
  let element3 = 30
  let element4 = 40
  let element5 = 50
  
  let sum1 = element1
  let sum2 = add(sum1, element2)
  let sum3 = add(sum2, element3)
  let sum4 = add(sum3, element4)
  let sum5 = add(sum4, element5)
  
  // 计算平均值
  let count = 5
  let average = divide_with_ceil(sum5, count)
  
  assert_eq(10, sum1)
  assert_eq(30, sum2)
  assert_eq(60, sum3)
  assert_eq(100, sum4)
  assert_eq(150, sum5)
  assert_eq(30, average)
}

test "factorial_calculation" {
  // 阶乘计算测试
  // 计算 6! = 720
  
  let fact1 = 1  // 1! = 1
  let fact2 = multiply(fact1, 2)  // 2! = 2
  let fact3 = multiply(fact2, 3)  // 3! = 6
  let fact4 = multiply(fact3, 4)  // 4! = 24
  let fact5 = multiply(fact4, 5)  // 5! = 120
  let fact6 = multiply(fact5, 6)  // 6! = 720
  
  assert_eq(1, fact1)
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
  assert_eq(720, fact6)
}

test "triangle_calculations" {
  // 三角形计算测试
  // 直角三角形，边长为 3, 4, 5
  
  let side_a = 3
  let side_b = 4
  let side_c = 5
  
  // 计算周长
  let perimeter = add(add(side_a, side_b), side_c)
  
  // 计算面积（使用海伦公式的简化版本）
  let s = divide_with_ceil(perimeter, 2)  // 半周长
  // 简化面积计算：底 × 高 ÷ 2
  let area = divide_with_ceil(multiply(side_a, side_b), 2)
  
  assert_eq(12, perimeter)
  assert_eq(6, s)
  assert_eq(6, area)
}

test "temperature_calculations" {
  // 温度计算测试
  let celsius = 30
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 华氏度转摄氏度（简化）：C = (F - 32) × 5/9
  let celsius_back = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  
  // 开尔文温度（简化）
  let kelvin = add(celsius, 273)
  
  assert_eq(86, fahrenheit)
  assert_eq(30, celsius_back)
  assert_eq(303, kelvin)
}

test "shopping_cart_calculations" {
  // 购物车计算测试
  let item1_price = 12
  let item1_qty = 3
  let item2_price = 8
  let item2_qty = 5
  let item3_price = 15
  let item3_qty = 2
  
  // 计算各商品小计
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let subtotal3 = multiply(item3_price, item3_qty)
  
  // 计算商品总计
  let items_total = add(add(subtotal1, subtotal2), subtotal3)
  
  // 计算折扣（10%折扣，向上取整）
  let discount = divide_with_ceil(multiply(items_total, 10), 100)
  
  // 计算最终价格
  let final_total = subtract(items_total, discount)
  
  assert_eq(36, subtotal1)
  assert_eq(40, subtotal2)
  assert_eq(30, subtotal3)
  assert_eq(106, items_total)
  assert_eq(11, discount)
  assert_eq(95, final_total)
}