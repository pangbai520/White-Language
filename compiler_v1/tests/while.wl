// tests/control_flow_complex.wl
let i -> Int = 0;
let sum -> Int = 0;

while (i < 10) {
    i = i + 1;
    
    // 测试 continue
    if (i == 5) {
        continue; // 跳过 5
    }
    
    // 测试 break
    if (i > 8) {
        break;
    }
    
    sum = sum + i;
}

// 预期：
// i=1, sum=1
// i=2, sum=3
// i=3, sum=6
// i=4, sum=10
// i=5, continue (sum不变)
// i=6, sum=16
// i=7, sum=23
// i=8, sum=31
// i=9, break

sum; // 预期输出: 31