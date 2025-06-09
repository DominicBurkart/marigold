use marigold::m;
use marigold::marigold_impl::StreamExt;

#[tokio::main]
async fn main() {
    println!("Testing minimal fold...");
    
    // Test 1: Basic fold with no post-transformations - should use simple case
    let result1 = m!(
        fn add(acc: u32, item: u32) -> u32 %%%MARIGOLD_FUNCTION_START%%%
            acc + item
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 4)
            .fold(0, add)
            .return
    ).await.collect::<Vec<u32>>().await;
    
    println!("Simple fold result: {:?}", result1);
    assert_eq!(result1, vec![6]);
    
    // Test 2: Post-fold transformation - should use complex case
    let result2 = m!(
        fn sum(acc: i32, item: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%%
            acc + item
        %%%MARIGOLD_FUNCTION_END%%%
        
        fn double(x: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%%
            x * 2
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 4)  // [1, 2, 3] -> sum = 6
            .fold(0, sum)    // 6
            .map(double)     // 12
            .return
    ).await.collect::<Vec<i32>>().await;
    
    println!("Complex fold result: {:?}", result2);
    assert_eq!(result2, vec![12]);
    
    println!("✅ Both tests passed!");
}