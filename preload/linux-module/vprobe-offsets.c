/*
 * vprobe-offset.c --- Module to dump offsets of fields from task_struct and
 *                     the current task_struct.
 */

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/mount.h>
#include <linux/fs.h>
#include <linux/proc_fs.h>
#include <linux/seq_file.h>
#include <linux/sched.h>
#include <linux/version.h>
#if   LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 33)
#include <generated/utsrelease.h>
#elif LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 18)
#include <linux/utsrelease.h>
#else
/* Prior to 2.6.18, version.h has the UTS_RELEASE definition */
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 11)
#error "vprobe-offsets module is not supported on kernels earlier than 2.6.11"
#endif

/*
 * Depending on the kernel version, curthrptr gets a pointer to either the,
 * thread_info, the pda or the per cpu area for each cpu.
 * task_offset contains the offset to where the current task_struct is stored
 * in the curthrptr area.
 */
#ifdef CONFIG_X86_32
   #define MEMMODEL "guest32"

   #if LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 22)
      #define curthrptr "FSBASE"
      #if LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 34)
         #define task_offset &current_task
      #else /* 2.6.22 to 2.6.33 */
         #if LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 25)
            #define per_cpu_var(var) per_cpu__##var
         #endif
         #define task_offset &per_cpu_var(current_task)
      #endif
   #elif LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 20)
      /* No swapgs for i386 */
      #define curthrptr "GSBASE"
      #define task_offset offsetof(struct i386_pda, pcurrent)
   #else
      /* FIXME:This doesn't work in user context */
      #if THREAD_SIZE == 4096
         #define curthrptr "(RSP & 0xfffff000)"
      #elif THREAD_SIZE == 8192
         #define curthrptr "(RSP & 0xffffe000)"
      #else
         #error "Invalid thread size"
      #endif
      #define task_offset 0
   #endif /* LINUX_VERSION_CODE */
#else /* !X86_32 */
   #define MEMMODEL "guest64"

   /*
    * This might be either GSBASE or KERNELGSBASE; testing the CPL isn't
    * *quite* right, because there's a short window immediately after the
    * hardware syscall where the right value is still in KERNELGSBASE,
    * i.e. before swapgs.
    */
   #define curthrptr "(GSBASE >= 0x100000000 ? GSBASE : KERNELGSBASE)"

   #if LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 33)
      #define task_offset &current_task
   #elif LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 30)
      #define task_offset &per_cpu_var(current_task)
   #else
      #define task_offset offsetof(struct x8664_pda, pcurrent)
   #endif /* LINUX_VERSION_CODE */
#endif /* X86_32 */

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 26)
    /*
     * Linux versions back to 2.6.20 should be compatible with the
     * core file-related stuff. (struct mm_struct).exe_file is new in
     * 2.6.26, though. For simplicity, just use one version everything.
     */
    #define HAVE_LINUX_FILES 1
    #if LINUX_VERSION_CODE >= KERNEL_VERSION(3, 3, 0)
        struct mount {
                struct list_head mnt_hash;
                struct mount *mnt_parent;
                struct dentry *mnt_mountpoint;
                struct vfsmount mnt;
                /* more */
        };
        #define __OFF_VFSMOUNT  (offsetof(struct mount, mnt))
    #else
        #define __OFF_VFSMOUNT  0
    #endif
#endif

static int vprobe_offsets_show(struct seq_file* m, void *v)
{
   seq_printf(m,
              "/* Linux offsets for kernel version %s */\n"
              "memmodel %s;\n"

              #ifdef HAVE_LINUX_FILES
              "\n#define HAVE_LINUX_FILES   1\n"
              "/* struct dentry */\n"
              "#define __OFF_DENTRY_PARENT  0x%p\n"
              "#define __OFF_DENTRY_NAME    0x%p\n"
              "/* struct file / struct path */\n"
              "#define __OFF_FILE_PATH      0x%p\n"
              "/* struct vfsmount / mount */\n"
              "#define __OFF_VFSMOUNT       0x%x\n"

              "\n/* struct mm_struct */\n"
              "#define __OFF_EXE    0x%p\n"
              #endif

              "\n/* struct task_struct */\n"
              "#define __OFF_TGID   0x%p\n"
              "#define __OFF_PID    0x%p\n"
              "#define __OFF_PIDS   0x%p\n"
              "#define __OFF_COMM   0x%p\n"
              "#define __OFF_PARENT 0x%p\n"
              "#define __OFF_MM     0x%p\n"

              "\n/* struct current_thread */\n"
              "#define __OFF_TASK   0x%p\n"
              "#define __CURTHRPTR  %s\n"
              ,
              UTS_RELEASE,
              MEMMODEL,
              #ifdef HAVE_LINUX_FILES
              (void*) offsetof(struct dentry, d_parent),
              (void*) offsetof(struct dentry, d_name),
              (void*) offsetof(struct file, f_path),
              (void*) __OFF_VFSMOUNT,
              (void*) offsetof(struct mm_struct, exe_file),
              #endif
              (void*) offsetof(struct task_struct, tgid),
              (void*) offsetof(struct task_struct, pid),
              (void*) offsetof(struct task_struct, pids),
              (void*) offsetof(struct task_struct, comm),
              (void*) offsetof(struct task_struct, parent),
              (void*) offsetof(struct task_struct, mm),
              (void*) task_offset,
              curthrptr);

   return 0;
}

static int vprobe_offsets_open(struct inode * inode, struct file * file)
{
   return single_open(file, vprobe_offsets_show, NULL);
}

static const struct file_operations vprobe_offsets_fops = {
   .open           = vprobe_offsets_open,
   .read           = seq_read,
   .llseek         = seq_lseek,
   .release        = single_release,
};

static int __init vprobe_offsets_init(void)
{
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2, 6, 24)
   struct proc_dir_entry *p;
   p = create_proc_entry("vprobe-offsets", 0, NULL);
   p->proc_fops = &vprobe_offsets_fops;
#else
   proc_create("vprobe-offsets", 0, NULL, &vprobe_offsets_fops);
#endif
   printk(KERN_INFO "Loaded vprobe-offsets.ko");
   return 0;
}
module_init(vprobe_offsets_init);

static void __exit vprobe_offsets_exit(void)
{
   remove_proc_entry("vprobe-offsets", NULL);
   printk(KERN_INFO "Unloading vprobe-offsets.ko");
}
module_exit(vprobe_offsets_exit);
MODULE_LICENSE("GPL");
