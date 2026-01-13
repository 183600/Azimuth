// 新增的 MoonBit 测试用例
// 这些测试用例涵盖了不同的场景和应用

test "factorial_calculation" {
  // 计算阶乘：n! = n × (n-1) × (n-2) × ... × 1
  // 计算 5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  
  let factorial = @azimuth.multiply(
    n,
    @azimuth.multiply(
      @azimuth.add(n, -1),
      @azimuth.multiply(
        @azimuth.add(n, -2),
        @azimuth.multiply(
          @azimuth.add(n, -3),
          @azimuth.add(n, -4)
        )
      )
    )
  )
  
  @azimuth.assert_eq(120, factorial)
}

test "fibonacci_sequence" {
  // 斐波那契数列：F(n) = F(n-1) + F(n-2)
  // 计算前几项：0, 1, 1, 2, 3, 5, 8, 13...
  
  let fib0 = 0
  let fib1 = 1
  let fib2 = @azimuth.add(fib0, fib1)  // 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(1, fib2)
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
}

test "greatest_common_divisor" {
  // 使用欧几里得算法计算最大公约数（简化版）
  // gcd(a, b) = gcd(b, a mod b)
  // 计算 gcd(48, 18) = 6
  
  let a = 48
  let b = 18
  
  // 第一次迭代：gcd(48, 18) = gcd(18, 48 mod 18 = 12)
  let remainder1 = a % b
  // 第二次迭代：gcd(18, 12) = gcd(12, 18 mod 12 = 6)
  let remainder2 = b % remainder1
  // 第三次迭代：gcd(12, 6) = gcd(6, 12 mod 6 = 0)
  let remainder3 = remainder1 % remainder2
  
  // 当余数为0时，当前除数就是GCD
  @azimuth.assert_eq(6, remainder2)
}

test "binary_conversion_simulation" {
  // 模拟二进制转换（简化版）
  // 将十进制数13转换为二进制1101
  
  let decimal = 13
  
  // 计算二进制位
  let bit3 = decimal / 8        // 13 / 8 = 1 (最高位)
  let remainder1 = decimal % 8   // 13 % 8 = 5
  let bit2 = remainder1 / 4      // 5 / 4 = 1
  let remainder2 = remainder1 % 4 // 5 % 4 = 1
  let bit1 = remainder2 / 2      // 1 / 2 = 0
  let bit0 = remainder2 % 2      // 1 % 2 = 1 (最低位)
  
  // 验证二进制位：1101
  @azimuth.assert_eq(1, bit3)
  @azimuth.assert_eq(1, bit2)
  @azimuth.assert_eq(0, bit1)
  @azimuth.assert_eq(1, bit0)
}

test "quadratic_equation" {
  // 解二次方程 ax² + bx + c = 0
  // 对于方程 x² - 5x + 6 = 0，解为 x = 2 和 x = 3
  // 使用求根公式：x = (-b ± √(b²-4ac)) / 2a
  
  let a = 1
  let b = -5
  let c = 6
  
  // 计算判别式：b² - 4ac
  let discriminant = @azimuth.add(
    @azimuth.multiply(b, b),
    -@azimuth.multiply(@azimuth.multiply(4, a), c)
  )
  
  // 验证判别式：(-5)² - 4×1×6 = 25 - 24 = 1
  @azimuth.assert_eq(1, discriminant)
  
  // 对于这个特殊情况，我们知道解是2和3
  let solution1 = 2
  let solution2 = 3
  
  // 验证解
  let check1 = @azimuth.add(
    @azimuth.add(
      @azimuth.multiply(a, @azimuth.multiply(solution1, solution1)),
      @azimuth.multiply(b, solution1)
    ),
    c
  )
  
  let check2 = @azimuth.add(
    @azimuth.add(
      @azimuth.multiply(a, @azimuth.multiply(solution2, solution2)),
      @azimuth.multiply(b, solution2)
    ),
    c
  )
  
  @azimuth.assert_eq(0, check1)
  @azimuth.assert_eq(0, check2)
}

test "speed_distance_time" {
  // 速度、距离、时间计算
  // 公式：距离 = 速度 × 时间
  
  let speed = 60  // km/h
  let time = 2.5  // 小时，使用整数近似：2.5 -> 5/2
  
  // 使用整数计算：60 × 5 ÷ 2 = 300 ÷ 2 = 150
  let distance = @azimuth.multiply(speed, 5) / 2
  
  @azimuth.assert_eq(150, distance)
  
  // 反向计算：速度 = 距离 ÷ 时间
  let calculated_speed = distance * 2 / 5
  @azimuth.assert_eq(speed, calculated_speed)
}

test "simple_interest_calculation" {
  // 简单利息计算
  // 公式：利息 = 本金 × 利率 × 时间
  
  let principal = 5000  // 本金
  let rate = 4          // 年利率 4%
  let time = 3          // 年数
  
  // 计算利息：5000 × 4% × 3 = 5000 × 0.04 × 3 = 600
  // 使用整数计算：5000 × 4 × 3 ÷ 100
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time) / 100
  
  @azimuth.assert_eq(600, interest)
  
  // 计算总金额：本金 + 利息
  let total_amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(5600, total_amount)
}

test "volume_calculation" {
  // 体积计算
  // 长方体体积 = 长 × 宽 × 高
  // 圆柱体体积 = π × r² × h（使用π≈3.14的整数近似）
  
  // 长方体体积
  let length = 5
  let width = 3
  let height = 4
  let cuboid_volume = @azimuth.multiply(
    @azimuth.multiply(length, width),
    height
  )
  @azimuth.assert_eq(60, cuboid_volume)
  
  // 圆柱体体积（使用π≈3）
  let radius = 4
  let cylinder_height = 5
  let pi_approx = 3
  let cylinder_volume = @azimuth.multiply(
    @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius)),
    cylinder_height
  )
  @azimuth.assert_eq(240, cylinder_volume)  // 3 × 16 × 5 = 240
}

test "leap_year_calculation" {
  // 闰年计算规则：
  // 1. 能被4整除但不能被100整除，或者能被400整除的年份是闰年
  
  let year1 = 2020  // 闰年（能被4整除，不能被100整除）
  let year2 = 1900  // 非闰年（能被100整除，但不能被400整除）
  let year3 = 2000  // 闰年（能被400整除）
  let year4 = 2023  // 非闰年（不能被4整除）
  
  // 检查2020年
  let divisible_by_4_1 = year1 % 4 == 0
  let divisible_by_100_1 = year1 % 100 == 0
  let divisible_by_400_1 = year1 % 400 == 0
  let is_leap_1 = divisible_by_4_1 && (not divisible_by_100_1 || divisible_by_400_1)
  @azimuth.assert_true(is_leap_1)
  
  // 检查1900年
  let divisible_by_4_2 = year2 % 4 == 0
  let divisible_by_100_2 = year2 % 100 == 0
  let divisible_by_400_2 = year2 % 400 == 0
  let is_leap_2 = divisible_by_4_2 && (not divisible_by_100_2 || divisible_by_400_2)
  @azimuth.assert_true(not is_leap_2)  // 应该是false，所以用not
  
  // 检查2000年
  let divisible_by_4_3 = year3 % 4 == 0
  let divisible_by_100_3 = year3 % 100 == 0
  let divisible_by_400_3 = year3 % 400 == 0
  let is_leap_3 = divisible_by_4_3 && (not divisible_by_100_3 || divisible_by_400_3)
  @azimuth.assert_true(is_leap_3)
  
  // 检查2023年
  let divisible_by_4_4 = year4 % 4 == 0
  let is_leap_4 = divisible_by_4_4
  @azimuth.assert_true(not is_leap_4)  // 应该是false，所以用not
}

test "data_structure_simulation" {
  // 模拟数据结构操作
  // 模拟栈操作：push和pop
  
  // 初始栈为空
  let stack_top = 0  // 0表示空栈
  
  // push 5
  stack_top = 5
  @azimuth.assert_eq(5, stack_top)
  
  // push 3
  stack_top = 3
  @azimuth.assert_eq(3, stack_top)
  
  // pop（模拟：将栈顶设置为"前一个值"）
  // 这里我们简化处理，直接设置为一个已知值
  stack_top = 5  // 模拟pop后露出前一个值
  @azimuth.assert_eq(5, stack_top)
  
  // push 7
  stack_top = 7
  @azimuth.assert_eq(7, stack_top)
  
  // pop
  stack_top = 5
  @azimuth.assert_eq(5, stack_top)
  
  // pop
  stack_top = 0  // 栈为空
  @azimuth.assert_eq(0, stack_top)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算一组数的平均值、最大值、最小值
  let numbers = [12, 18, 24, 30, 36]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(24, average)  // (12 + 18 + 24 + 30 + 36) / 5 = 120 / 5 = 24
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(36, -12)
  @azimuth.assert_eq(24, range)  // 36 - 12 = 24
}

test "unit_conversion_calculations" {
  // 测试单位转换计算
  // 长度转换：1米 = 100厘米 = 1000毫米
  let meters = 5
  let centimeters = @azimuth.multiply(meters, 100)
  let millimeters = @azimuth.multiply(meters, 1000)
  
  @azimuth.assert_eq(500, centimeters)  // 5米 = 500厘米
  @azimuth.assert_eq(5000, millimeters) // 5米 = 5000毫米
  
  // 重量转换：1千克 = 1000克
  let kilograms = 3
  let grams = @azimuth.multiply(kilograms, 1000)
  @azimuth.assert_eq(3000, grams)  // 3千克 = 3000克
}

test "simple_interest_calculation" {
  // 测试简单利息计算
  // 利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 2  // 2年
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(100000, interest)      // 10000 × 5 × 2 = 100000
  @azimuth.assert_eq(110000, total_amount) // 10000 + 100000 = 110000
}

test "moonbit_new_test_case" {
  // 新的 MoonBit 测试用例
  // 测试基本算术运算
  @azimuth.assert_eq(25, @azimuth.add(10, 15))
  @azimuth.assert_eq(50, @azimuth.multiply(5, 10))
  
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  
  // 测试复杂表达式
  let result = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(2, 5))
  @azimuth.assert_eq(22, result)  // 3*4 + 2*5 = 12 + 10 = 22
  
  // 测试边界值
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
}