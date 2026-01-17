// Azimuth 项目额外单元测试
// 使用标准 MoonBit 测试语法，补充更多测试场景

test "large_number_operations" {
  // 测试大数运算
  @azimuth.assert_eq(3000000, @azimuth.add(1000000, 2000000))
  @azimuth.assert_eq(1000000000, @azimuth.add(999999999, 1))
  @azimuth.assert_eq(10000000, @azimuth.multiply(10000, 1000))
  @azimuth.assert_eq(83810205, @azimuth.multiply(12345, 6789))
}

test "negative_number_edge_cases" {
  // 测试负数边界情况
  @azimuth.assert_eq(0, @azimuth.add(-1000000, 1000000))
  @azimuth.assert_eq(-1000, @azimuth.add(-500, -500))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(10000, @azimuth.multiply(-100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(-100, 50))
}

test "string_processing_variations" {
  // 测试字符串处理的各种情况
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob Smith!", @azimuth.greet("Bob Smith"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello,    !", @azimuth.greet("   "))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 15
  let b = 25
  let c = 10
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律: a * (b + c) = a * b + a * c
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "shopping_cart_calculations" {
  // 测试购物车计算场景
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  let tax_rate = 10  // 10%
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  @azimuth.assert_eq(75, item1_total)
  @azimuth.assert_eq(30, item2_total)
  @azimuth.assert_eq(105, subtotal)
  
  // 计算税费和总价
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(subtotal, tax_amount)
  
  @azimuth.assert_eq(10, tax_amount)
  @azimuth.assert_eq(115, total)
}

test "scientific_calculations" {
  // 测试科学计算场景
  
  // 计算速度: 距离 / 时间
  let distance = 300  // 公里
  let time = 5  // 小时
  let speed = @azimuth.multiply(distance, 1) / time
  @azimuth.assert_eq(60, speed)
  
  // 计算密度: 质量 / 体积
  let mass = 800  // 克
  let volume = 2  // 立方厘米
  let density = @azimuth.multiply(mass, 1) / volume
  @azimuth.assert_eq(400, density)
  
  // 计算动能: 0.5 * 质量 * 速度^2
  let kinetic_mass = 10  // kg
  let velocity = 5  // m/s
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(kinetic_mass, velocity), velocity) / 2
  @azimuth.assert_eq(125, kinetic_energy)
}

test "unit_conversion_scenarios" {
  // 测试单位转换场景
  
  // 公里转米: 公里 * 1000
  let kilometers = 5
  let meters = @azimuth.multiply(kilometers, 1000)
  @azimuth.assert_eq(5000, meters)
  
  // 千克转克: 千克 * 1000
  let kilograms = 3
  let grams = @azimuth.multiply(kilograms, 1000)
  @azimuth.assert_eq(3000, grams)
  
  // 小时转秒: 小时 * 3600
  let hours = 2
  let seconds = @azimuth.multiply(hours, 3600)
  @azimuth.assert_eq(7200, seconds)
  
  // 升转毫升: 升 * 1000
  let liters = 4
  let milliliters = @azimuth.multiply(liters, 1000)
  @azimuth.assert_eq(4000, milliliters)
}

test "boundary_value_testing" {
  // 测试边界值
  
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  
  // 测试单位元
  @azimuth.assert_eq(12345, @azimuth.add(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  
  // 测试最小值
  @azimuth.assert_eq(0, @azimuth.add(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  
  // 计算员工工资: 基本工资 + 加班费 - 扣除
  let base_salary = 3000
  let overtime_hours = 10
  let overtime_rate = 50  // 每小时加班费
  let deductions = 200
  
  let overtime_pay = @azimuth.multiply(overtime_hours, overtime_rate)
  let gross_salary = @azimuth.add(base_salary, overtime_pay)
  let net_salary = @azimuth.add(gross_salary, -deductions)
  
  @azimuth.assert_eq(500, overtime_pay)
  @azimuth.assert_eq(3500, gross_salary)
  @azimuth.assert_eq(3300, net_salary)
  
  // 计算投资回报: 本金 + 本金 * 利率 * 时间
  let principal = 10000
  let interest_rate = 5  // 5%
  let investment_years = 3
  
  let interest = @azimuth.multiply(@azimuth.multiply(principal, interest_rate), investment_years) / 100
  let final_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(1500, interest)
  @azimuth.assert_eq(11500, final_amount)
}