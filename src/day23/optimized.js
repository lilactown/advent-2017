    let a =0, b=0, c=0, d=0, e=0, f=0, g=0, h=0;

    b = 84;
    c = b;
    if (a !== 0) {
        b = b * 100;
        b -= -100000;
        c = b;
        c -= -17000;
    }
    while (true) {
        f = 1;
        d = 2;
        do {
            e = 2;
            do {
                g = d;
                g = g * e;
                g -= b;
                if (g === 0) {
                    f = 0;
                }
                e -= -1;
                g = e;
                g -= b;
            } while (g !== 0);
            d -= -1;
            g = d;
            g -= b;
        } while (g !== 0);
        if (f == 0) {
            h -= -1;
        }
        g = b;
        g -= c;
        if (g == 0) {
            break;
        }
        b -= -17;
    }


    // Step 1
    //
    // let a=1, b=0, c=0, d=0, e=0, f=0, g=0, h=0;
    // b = 108400;
    // c = 125400;
    // while (true) {
    //     f = 1;
    //     d = 2;
    //     do {
    //         e = 2;
    //         do {
    //             if (d * e - b === 0) {
    //                 f = 0;
    //             }
    //             e++;
    //             g = e - b;
    //         } while (g !== 0);
    //         d++;
    //         g = d - b;
    //     } while (g !== 0);
    //     if (f == 0) {
    //         h++;
    //     }
    //     g = b - c;
    //     if (g == 0) {
    //         break;
    //     }
    //     b += 17;
    // }

    // Step 2
    //
    // let h = 0;
    // for (let b = 108400; b <= 125400; b += 17) {
    //     for (let d = 2; d <= b; d++) {
    //         for (let e = 2; e <= b; e++) {
    //             if (d * e === b) {
    //                 f = 0
    //             }
    //         }
    //     }
    //     if (f == 0) {
    //         h += 1;
    //     }
    // }

    // Step 3
    //
    // let h = 0;
    // for (let n = 108400; n <= 125400; b+= 17) {
    //     if (!prime(n)) {
    //         h+= 1
    //     }
    // }
