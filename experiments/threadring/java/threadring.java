/**

 * The Computer Language Benchmarks Game

 * http://benchmarksgame.alioth.debian.org/

 * contributed by Klaus Friedel

 * modified by Daryl Griffith

 */

import java.util.concurrent.locks.LockSupport;

public class threadring extends Thread {

    static final int THREAD_COUNT = 503;

    threadring nextThread;
    volatile boolean waiting = true;
    int message;

    public threadring(int name) {
        super(Integer.toString(name));
    }

    @Override
    public void run() {
        for (;;) {
            while (waiting) {
                LockSupport.park();
            }
            if (message == 0) {
                System.out.println(getName());
                System.exit(0);
            }
            waiting = true;
            nextThread.message = message - 1;
            nextThread.waiting = false;
            LockSupport.unpark(nextThread);
        }
    }

    public static void main(String args[]) throws Exception {
        threadring first = new threadring(1);
        threadring current = new threadring(2);

        first.start(); // Thread 1

        first.nextThread = current;
        first.message = Integer.parseInt(args[0]);
        first.waiting = false;
        for (int i = 3; i < THREAD_COUNT; i++) {
            current.nextThread = new threadring(i);
            current.start();
            current = current.nextThread;
        }
        current.nextThread = new threadring(THREAD_COUNT);
        current.start(); // Thread 502

        current = current.nextThread;
        current.nextThread = first;
        current.start(); // Thread 503

    }
}
