#include "system.h"
#ifdef PORTABLE_BYTECODE

#include <string.h>
#include <math.h>

/* Interpreter for portable bytecode. See also "pb.ss", while
   instruction implementations are mostly in "pb.h" */
#include "pb.h"

typedef uptr (*chunk_t)(machine_state *ms, uptr, int);

static chunk_t *chunks;
static int num_chunks;

#ifdef WASM_PBCHUNK
extern uptr wasm_do_jump(int jump_idx, machine_state *ms, uptr ip);
#endif

#ifdef CALL_PBCHUNK_REGISTER
extern void pbchunk_register();
void S_machine_init() { pbchunk_register(); }
#else

#ifdef CALL_WASM_PBCHUNK_REGISTER
extern void wasm_pbchunk_register();
void S_machine_init() { wasm_pbchunk_register(); }

#else

# ifndef FEATURE_WINDOWS
void S_machine_init() { }
# endif

#endif

#endif

const char *reg_names[] = {
  "tc",
  "sfp",
  "ap",
  "trap",
  "ac0",
  "xp",
  "ts",
  "td",
  "cp",
  "r9",
  "r10",
  "r11",
  "r12",
  "r13",
  "r14",
  "r15"
};

void dump_machine_state(machine_state *ms) {
  printf("| registers |\n");
  for (int i = 0; i < pb_reg_count; i++) {
    printf("\t%s = %llx\n", reg_names[i], ms->machine_regs[i]);
  }
}

void Sregister_pbchunks(void **add_chunks, int start_index, int end_index) {
  if (num_chunks < end_index) {
    void *new_chunks = malloc(sizeof(void*) * end_index);
    if (chunks) {
      memcpy(new_chunks, chunks, num_chunks * sizeof(void*));
      free(chunks);
    }
    chunks = new_chunks;
    num_chunks = end_index;
  }

  memcpy((void **)chunks + start_index, add_chunks, (end_index - start_index) * sizeof(void*));
}

#if 0
# define TRACE(print, record) print
#elif 0
# define TRACE(print, record) record
static instruction_t *branch_from, *branch_to;
static instruction_t *jump_from, *jump_to;
static instruction_t *interp_from, *interp_to;
static instruction_t *call_from; static void *call_to;
#else
# define TRACE(print, record) /* empty */
#endif

//#define COMMON_INSTR(x, tag) x: doi_ ## x(instr); printf("instruction: %d\n", tag); break;
#define COMMON_INSTR(x, tag) x: doi_ ## x(instr); break;

void S_pb_interp(ptr tc, void *bytecode) {
  machine_state * RESTRICT_PTR ms = (machine_state *)&PBREGS(tc, 0); /* assumes fields are together in `tc` */
  instruction_t *ip = (instruction_t *)bytecode, *next_ip, instr;
  int flag = 0;

  regs[0] = (uptr)tc;

  TRACE(printf("enter %p\n", ip), );

  while (1) {
    instr = *ip;
    next_ip = ip + 1;

    switch(INSTR_op(instr)) {
    case pb_nop:
      break;
    case pb_literal:
      regs[INSTR_di_dest(instr)] = (ptr)LOAD_UNALIGNED_UPTR(ip + 1); 
#if ptr_bits == 64
      next_ip = ip + 3;
#else
      next_ip = ip + 2;
#endif
      break;
    case COMMON_INSTR(pb_mov16_pb_zero_bits_pb_shift0, 0xA1)
    case COMMON_INSTR(pb_mov16_pb_zero_bits_pb_shift1, 0xA2)
    case COMMON_INSTR(pb_mov16_pb_zero_bits_pb_shift2, 0xA3)
    case COMMON_INSTR(pb_mov16_pb_zero_bits_pb_shift3, 0xA4)
    case COMMON_INSTR(pb_mov16_pb_keep_bits_pb_shift0, 0xA5)
    case COMMON_INSTR(pb_mov16_pb_keep_bits_pb_shift1, 0xA6)
    case COMMON_INSTR(pb_mov16_pb_keep_bits_pb_shift2, 0xA7)
    case COMMON_INSTR(pb_mov16_pb_keep_bits_pb_shift3, 0xA8)
    case COMMON_INSTR(pb_mov_pb_i_i, 0xA9)
    case COMMON_INSTR(pb_mov_pb_d_d, 0xAA)
    case COMMON_INSTR(pb_mov_pb_i_d, 0xAB)
    case COMMON_INSTR(pb_mov_pb_d_i, 0xAC)
#if ptr_bits == 64
    case COMMON_INSTR(pb_mov_pb_i_bits_d_bits, 0xAD)
    case COMMON_INSTR(pb_mov_pb_d_bits_i_bits, 0xAE)
#else
    case COMMON_INSTR(pb_mov_pb_i_i_bits_d_bits, 0xAF)
    case COMMON_INSTR(pb_mov_pb_d_lo_bits_i_bits, 0xB0)
    case COMMON_INSTR(pb_mov_pb_d_hi_bits_i_bits, 0xB1)
#endif      
    case COMMON_INSTR(pb_mov_pb_s_d, 0xB2)
    case COMMON_INSTR(pb_mov_pb_d_s, 0xB3)
    case COMMON_INSTR(pb_mov_pb_d_s_d, 0xB4)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_add_pb_register, 0xB5)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_add_pb_immediate, 0xB6)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_sub_pb_register, 0xB7)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_sub_pb_immediate, 0xB8)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_mul_pb_register, 0xB9)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_mul_pb_immediate, 0xBA)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_div_pb_register, 0xBB)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_div_pb_immediate, 0xBC)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_and_pb_register, 0xBD)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_and_pb_immediate, 0xBE)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_ior_pb_register, 0xBF)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_ior_pb_immediate, 0xC0)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_xor_pb_register, 0xC1)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_xor_pb_immediate, 0xC2)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_lsl_pb_register, 0xC3)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_lsl_pb_immediate, 0xC4)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_lsr_pb_register, 0xC5)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_lsr_pb_immediate, 0xC6)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_asr_pb_register, 0xC7)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_asr_pb_immediate, 0xC8)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_lslo_pb_register, 0xC9)
    case COMMON_INSTR(pb_bin_op_pb_no_signal_pb_lslo_pb_immediate, 0xCA)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_add_pb_register, 0xCB)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_add_pb_immediate, 0xCC)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_sub_pb_register, 0xCD)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_sub_pb_immediate, 0xCE)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_mul_pb_register, 0xCF)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_mul_pb_immediate, 0xD0)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_subz_pb_register, 0xD1)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_subz_pb_immediate, 0xD2)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_subp_pb_register, 0xD3)
    case COMMON_INSTR(pb_bin_op_pb_signal_pb_subp_pb_immediate, 0xD4)
    case COMMON_INSTR(pb_cmp_op_pb_eq_pb_register, 0xD5)
    case COMMON_INSTR(pb_cmp_op_pb_eq_pb_immediate, 0xD6)
    case COMMON_INSTR(pb_cmp_op_pb_lt_pb_register, 0xD7)
    case COMMON_INSTR(pb_cmp_op_pb_lt_pb_immediate, 0xD8)
    case COMMON_INSTR(pb_cmp_op_pb_gt_pb_register, 0xD9)
    case COMMON_INSTR(pb_cmp_op_pb_gt_pb_immediate, 0xDA)
    case COMMON_INSTR(pb_cmp_op_pb_le_pb_register, 0xDB)
    case COMMON_INSTR(pb_cmp_op_pb_le_pb_immediate, 0xDC)
    case COMMON_INSTR(pb_cmp_op_pb_ge_pb_register, 0xDE)
    case COMMON_INSTR(pb_cmp_op_pb_ge_pb_immediate, 0xDF)
    case COMMON_INSTR(pb_cmp_op_pb_ab_pb_register, 0xF0)
    case COMMON_INSTR(pb_cmp_op_pb_ab_pb_immediate, 0xF1)
    case COMMON_INSTR(pb_cmp_op_pb_bl_pb_register, 0xF2)
    case COMMON_INSTR(pb_cmp_op_pb_bl_pb_immediate, 0xF3)
    case COMMON_INSTR(pb_cmp_op_pb_cs_pb_register, 0xF4)
    case COMMON_INSTR(pb_cmp_op_pb_cs_pb_immediate, 0xF5)
    case COMMON_INSTR(pb_cmp_op_pb_cc_pb_register, 0xF6)
    case COMMON_INSTR(pb_cmp_op_pb_cc_pb_immediate, 0xF7)
    case COMMON_INSTR(pb_fp_bin_op_pb_add_pb_register, 0xF8)
    case COMMON_INSTR(pb_fp_bin_op_pb_sub_pb_register, 0xF9)
    case COMMON_INSTR(pb_fp_bin_op_pb_mul_pb_register, 0xF10)
    case COMMON_INSTR(pb_fp_bin_op_pb_div_pb_register, 0xFA)
    case COMMON_INSTR(pb_un_op_pb_not_pb_register, 0xFB)
    case COMMON_INSTR(pb_un_op_pb_not_pb_immediate, 0xFC)
    case COMMON_INSTR(pb_fp_un_op_pb_sqrt_pb_register, 0xFD)
    case COMMON_INSTR(pb_fp_cmp_op_pb_eq_pb_register, 0xFE)
    case COMMON_INSTR(pb_fp_cmp_op_pb_lt_pb_register, 0xFF)
    case COMMON_INSTR(pb_fp_cmp_op_pb_le_pb_register, 0x100)
    case COMMON_INSTR(pb_rev_op_pb_int16_pb_register, 0x101)
    case COMMON_INSTR(pb_rev_op_pb_uint16_pb_register, 0x102)
    case COMMON_INSTR(pb_rev_op_pb_int32_pb_register, 0x103)
    case COMMON_INSTR(pb_rev_op_pb_uint32_pb_register, 0x104)
    case COMMON_INSTR(pb_rev_op_pb_int64_pb_register, 0x105)
    case COMMON_INSTR(pb_ld_op_pb_int8_pb_register, 0x106)
    case COMMON_INSTR(pb_ld_op_pb_int8_pb_immediate, 0x107)
    case COMMON_INSTR(pb_ld_op_pb_uint8_pb_register, 0x108)
    case COMMON_INSTR(pb_ld_op_pb_uint8_pb_immediate, 0x109)
    case COMMON_INSTR(pb_ld_op_pb_int16_pb_register, 0x10A)
    case COMMON_INSTR(pb_ld_op_pb_int16_pb_immediate, 0x10B)
    case COMMON_INSTR(pb_ld_op_pb_uint16_pb_register, 0x10C)
    case COMMON_INSTR(pb_ld_op_pb_uint16_pb_immediate, 0x10D)
    case COMMON_INSTR(pb_ld_op_pb_int32_pb_register, 0x10E)
    case COMMON_INSTR(pb_ld_op_pb_int32_pb_immediate, 0x10F)
    case COMMON_INSTR(pb_ld_op_pb_uint32_pb_register, 0x110)
    case COMMON_INSTR(pb_ld_op_pb_uint32_pb_immediate, 0x11A)
    case COMMON_INSTR(pb_ld_op_pb_int64_pb_register, 0x11B)
    case COMMON_INSTR(pb_ld_op_pb_int64_pb_immediate, 0x11C)
    case COMMON_INSTR(pb_ld_op_pb_double_pb_register, 0x11D)
    case COMMON_INSTR(pb_ld_op_pb_double_pb_immediate, 0x11E)
    case COMMON_INSTR(pb_ld_op_pb_single_pb_register, 0x11F)
    case COMMON_INSTR(pb_ld_op_pb_single_pb_immediate, 0x120)
    case COMMON_INSTR(pb_st_op_pb_int8_pb_register, 0x121)
    case COMMON_INSTR(pb_st_op_pb_int8_pb_immediate, 0x122)
    case COMMON_INSTR(pb_st_op_pb_int16_pb_register, 0x123)
    case COMMON_INSTR(pb_st_op_pb_int16_pb_immediate, 0x124)
    case COMMON_INSTR(pb_st_op_pb_int32_pb_register, 0x125)
    case COMMON_INSTR(pb_st_op_pb_int32_pb_immediate, 0x126)
    case COMMON_INSTR(pb_st_op_pb_int64_pb_register, 0x127)
    case COMMON_INSTR(pb_st_op_pb_int64_pb_immediate, 0x128)
    case COMMON_INSTR(pb_st_op_pb_double_pb_register, 0x129)
    case COMMON_INSTR(pb_st_op_pb_double_pb_immediate, 0x130)
    case COMMON_INSTR(pb_st_op_pb_single_pb_register, 0x131)
    case COMMON_INSTR(pb_st_op_pb_single_pb_immediate, 0x132)
    case pb_b_op_pb_fals_pb_register:
      if (!flag) {
        next_ip = (instruction_t *)TO_VOIDP(regs[INSTR_dr_reg(instr)]);
        TRACE(printf("branch %p -> %p\n", ip, next_ip), { branch_from = ip; branch_to = next_ip; });
      }
      break;
    case pb_b_op_pb_fals_pb_immediate:
      if (!flag) {
        next_ip = (instruction_t *)TO_VOIDP((char *)next_ip + INSTR_i_imm(instr));
        TRACE(printf("branch %p -> %p\n", ip, next_ip), { branch_from = ip; branch_to = next_ip; });
      }
      break;
    case pb_b_op_pb_true_pb_register:
      if (flag) {
        next_ip = (instruction_t *)TO_VOIDP(regs[INSTR_dr_reg(instr)]);
        TRACE(printf("branch %p -> %p\n", ip, next_ip), { branch_from = ip; branch_to = next_ip; });
      }
      break;
    case pb_b_op_pb_true_pb_immediate:
      if (flag) {
        next_ip = (instruction_t *)TO_VOIDP((char *)next_ip + INSTR_i_imm(instr));
        TRACE(printf("branch %p -> %p\n", ip, next_ip), { branch_from = ip; branch_to = next_ip; });
      }
      break;
    case pb_b_op_pb_always_pb_register:
      next_ip = (instruction_t *)TO_VOIDP(regs[INSTR_dr_reg(instr)]);
      TRACE(printf("jump %p -> %p\n", ip, next_ip), { jump_from = ip; jump_to = next_ip; });
      break;
    case pb_b_op_pb_always_pb_immediate:
      next_ip = (instruction_t *)TO_VOIDP((char *)next_ip + INSTR_i_imm(instr));
      TRACE(printf("jump %p -> %p\n", ip, next_ip), { jump_from = ip; jump_to = next_ip; });
      break;
    case pb_bs_op_pb_register:
      next_ip = (instruction_t *)TO_VOIDP(geti_pb_bs_op_pb_register_addr(instr));
      TRACE(printf("jump %p -> %p\n", ip, next_ip), { jump_from = ip; jump_to = next_ip; });
      break;
    case pb_bs_op_pb_immediate:
      next_ip = (instruction_t *)TO_VOIDP(geti_pb_bs_op_pb_immediate_addr(instr));
      TRACE(printf("jump %p -> %p\n", ip, next_ip), { jump_from = ip; jump_to = next_ip; });
      break;
    case pb_return:
      return; /* <--- not break */
    case pb_adr:
      regs[INSTR_adr_dest(instr)] = (uptr)TO_PTR(next_ip) + (INSTR_adr_imm(instr) << 2);
      break;
    case pb_interp:
      {
        void *code = TO_VOIDP(regs[INSTR_d_dest(instr)]);
        TRACE(printf("interp %p -> %p\n", ip, code), { interp_from = ip; interp_to = (instruction_t *)regs[0]; });
        S_pb_interp((ptr)regs[0], code);
      }
      break;
    case pb_call:
      {
        void *proc = TO_VOIDP(regs[INSTR_dri_dest(instr)]);
        TRACE(printf("call %p -> %p %x\n", ip, proc, INSTR_dri_imm(instr)), { call_from = ip; call_to = proc; });
        switch (INSTR_dri_imm(instr)) {
        case pb_call_void:
          ((pb_void_t)proc)();
          break;
        case pb_call_void_uptr:
          ((pb_void_uptr_t)proc)(regs[Carg1]);
          break;
        case pb_call_void_int32:
          ((pb_void_int32_t)proc)((int32_t)regs[Carg1]);
          break;
        case pb_call_void_uint32:
          ((pb_void_uint32_t)proc)((uint32_t)regs[Carg1]);
          break;
        case pb_call_void_voids:
          ((pb_void_voids_t)proc)(TO_VOIDP(regs[Carg1]));
          break;
        case pb_call_void_uptr_uint32:
          ((pb_void_uptr_uint32_t)proc)(regs[Carg1], (uint32_t)regs[Carg2]);
          break;
        case pb_call_void_int32_uptr:
          ((pb_void_int32_uptr_t)proc)((int32_t)regs[Carg1], regs[Carg2]);
          break;
        case pb_call_void_int32_voids:
          ((pb_void_int32_voids_t)proc)((int32_t)regs[Carg1], TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_void_uptr_voids:
          ((pb_void_uptr_voids_t)proc)(regs[Carg1], TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_void_int32_int32:
          ((pb_void_int32_int32_t)proc)((int32_t)regs[Carg1], (int32_t)regs[Carg2]);
          break;
        case pb_call_void_uint32_uint32:
          ((pb_void_uint32_uint32_t)proc)((uint32_t)regs[Carg1], (uint32_t)regs[Carg2]);
          break;
        case pb_call_void_uptr_uptr:
          ((pb_void_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_void_voids_voids:
          ((pb_void_voids_voids_t)proc)(TO_VOIDP(regs[Carg1]), TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_void_uptr_uptr_uptr:
          ((pb_void_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3]);
          break;
        case pb_call_void_uptr_uptr_uptr_uptr_uptr:
          ((pb_void_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                     regs[Carg4], regs[Carg5]);
          break;
        case pb_call_int32:
          regs[Cretval] = ((pb_int32_t)proc)();
          break;
        case pb_call_int32_uptr:
          regs[Cretval] = ((pb_int32_uptr_t)proc)(regs[Carg1]);
          break;
        case pb_call_int32_voids:
          regs[Cretval] = ((pb_int32_voids_t)proc)(TO_VOIDP(regs[Carg1]));
          break;
        case pb_call_int32_uptr_int32:
          regs[Cretval] = ((pb_int32_uptr_int32_t)proc)(regs[Carg1], (int32_t)regs[Carg2]);
          break;
        case pb_call_int32_uptr_uptr:
          regs[Cretval] = ((pb_int32_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_int32_uptr_uptr_uptr:
          regs[Cretval] = ((pb_int32_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3]);
          break;
        case pb_call_int32_int32_int32:
          regs[Cretval] = ((pb_int32_int32_int32_t)proc)((int32_t)regs[Carg1], (int32_t)regs[Carg2]);
          break;
        case pb_call_int32_voids_int32:
          regs[Cretval] = ((pb_int32_voids_int32_t)proc)(TO_VOIDP(regs[Carg1]), (int32_t)regs[Carg2]);
          break;
        case pb_call_int32_int32_voids:
          regs[Cretval] = ((pb_int32_int32_voids_t)proc)((int32_t)regs[Carg1], TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_int32_double_double_double_double_double_double:
          regs[Cretval] = ((pb_int32_double_double_double_double_double_double_t)proc)(fpregs[Cfparg1], fpregs[Cfparg2], fpregs[Cfparg3],
                                                                                       fpregs[Cfparg4], fpregs[Cfparg5], fpregs[Cfparg6]);
          break;
        case pb_call_uint32:
          regs[Cretval] = ((pb_uint32_t)proc)();
          break;
        case pb_call_double_double:
          fpregs[Cfpretval] = ((pb_double_double_t)proc)(fpregs[Cfparg1]);
          break;
        case pb_call_double_uptr:
          fpregs[Cfpretval] = ((pb_double_uptr_t)proc)(regs[Carg1]);
          break;
        case pb_call_double_double_double:
          fpregs[Cfpretval] = ((pb_double_double_double_t)proc)(fpregs[Cfparg1], fpregs[Cfparg2]);
          break;
        case pb_call_int32_int32:
          regs[Cretval] = ((pb_int32_int32_t)proc)((int32_t)regs[Carg1]);
          break;
        case pb_call_int32_int32_uptr:
          regs[Cretval] = ((pb_int32_int32_uptr_t)proc)((int32_t)regs[Carg1], regs[Carg2]);
          break;
        case pb_call_int32_voids_voids_voids_voids_uptr:
          regs[Cretval] = ((pb_int32_voids_voids_voids_voids_uptr_t)proc)(TO_VOIDP(regs[Carg1]), TO_VOIDP(regs[Carg2]), TO_VOIDP(regs[Carg3]),
                                                                          TO_VOIDP(regs[Carg4]), regs[Carg5]);
          break;
        case pb_call_uptr:
          regs[Cretval] = ((pb_uptr_t)proc)();
          break;
        case pb_call_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_t)proc)(regs[Carg1]);
          break;
        case pb_call_uptr_int32:
          regs[Cretval] = ((pb_uptr_int32_t)proc)((int32_t)regs[Carg1]);
          break;
        case pb_call_uptr_voids:
          regs[Cretval] = ((pb_uptr_voids_t)proc)(TO_VOIDP(regs[Carg1]));
          break;
        case pb_call_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_uptr_uptr_int32:
          regs[Cretval] = ((pb_uptr_uptr_int32_t)proc)(regs[Carg1], (int32_t)regs[Carg2]);
          break;
        case pb_call_uptr_uptr_int64:
#if ptr_bits == 64
          regs[Cretval] = ((pb_uptr_uptr_int64_t)proc)(regs[Carg1], regs[Carg2]);
#else
          regs[Cretval] = ((pb_uptr_uptr_int64_t)proc)(regs[Carg1], regs[Carg2] | ((int64_t)regs[Carg3] << 32));
#endif
          break;
        case pb_call_uptr_int32_uptr:
          regs[Cretval] = ((pb_uptr_int32_uptr_t)proc)((int32_t)regs[Carg1], regs[Carg2]);
          break;
        case pb_call_uptr_voids_uptr:
          regs[Cretval] = ((pb_uptr_voids_uptr_t)proc)(TO_VOIDP(regs[Carg1]), regs[Carg2]);
          break;
        case pb_call_uptr_uptr_voids:
          regs[Cretval] = ((pb_uptr_uptr_voids_t)proc)(regs[Carg1], TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_uptr_voids_int32:
          regs[Cretval] = ((pb_uptr_voids_int32_t)proc)(TO_VOIDP(regs[Carg1]), (int32_t)regs[Carg2]);
          break;
        case pb_call_uptr_voids_voids:
          regs[Cretval] = ((pb_uptr_voids_voids_t)proc)(TO_VOIDP(regs[Carg1]), TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_uptr_uptr_int32_int32:
          regs[Cretval] = ((pb_uptr_uptr_int32_int32_t)proc)(regs[Carg1], (int32_t)regs[Carg2], (int32_t)regs[Carg3]);
          break;
        case pb_call_uptr_voids_int32_int32:
          regs[Cretval] = ((pb_uptr_voids_int32_int32_t)proc)(TO_VOIDP(regs[Carg1]), (int32_t)regs[Carg2], (int32_t)regs[Carg3]);
          break;
        case pb_call_uptr_voids_uptr_uptr:
          regs[Cretval] = ((pb_uptr_voids_uptr_uptr_t)proc)(TO_VOIDP(regs[Carg1]), regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_uptr_uptr_int32:
          regs[Cretval] = ((pb_uptr_uptr_uptr_int32_t)proc)(regs[Carg1], regs[Carg2], (int32_t)regs[Carg3]);
          break;
        case pb_call_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_int32_int32_uptr:
          regs[Cretval] = ((pb_uptr_int32_int32_uptr_t)proc)((int32_t)regs[Carg1], (int32_t)regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_int32_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_int32_uptr_uptr_uptr_t)proc)((int32_t)regs[Carg1], regs[Carg2], regs[Carg3],
                                                                 regs[Carg4]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                regs[Carg4]);
          break;
        case pb_call_uptr_int32_int32_uptr_uptr:
          regs[Cretval] = ((pb_uptr_int32_int32_uptr_uptr_t)proc)((int32_t)regs[Carg1], (int32_t)regs[Carg2], regs[Carg3],
                                                                  regs[Carg4]);
          break;
        case pb_call_uptr_int32_int32_int32_uptr:
          regs[Cretval] = ((pb_uptr_int32_int32_int32_uptr_t)proc)((int32_t)regs[Carg1], (int32_t)regs[Carg2], (int32_t)regs[Carg3],
                                                                   regs[Carg4]);
          break;
        case pb_call_uptr_int32_voids_uptr_uptr:
          regs[Cretval] = ((pb_uptr_int32_voids_uptr_uptr_t)proc)((int32_t)regs[Carg1], TO_VOIDP(regs[Carg2]), regs[Carg3],
                                                                  regs[Carg4]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_int32:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_int32_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                      regs[Carg4], (int32_t)regs[Carg5]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                     regs[Carg4], regs[Carg5]);
          break;
        case pb_call_uptr_voids_voids_voids_voids_uptr:
          regs[Cretval] = ((pb_uptr_voids_voids_voids_voids_uptr_t)proc)(TO_VOIDP(regs[Carg1]), TO_VOIDP(regs[Carg2]), TO_VOIDP(regs[Carg3]),
                                                                         TO_VOIDP(regs[Carg4]), regs[Carg5]);
          break;
        case pb_call_uptr_uptr_int32_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_int32_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], (int32_t)regs[Carg2], regs[Carg3],
                                                                           regs[Carg4], regs[Carg5], regs[Carg6]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                          regs[Carg4], regs[Carg5], regs[Carg6]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_uptr_uptr_int32:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_uptr_uptr_int32_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                                regs[Carg4], regs[Carg5], regs[Carg6],
                                                                                (int32_t)regs[Carg7]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                               regs[Carg4], regs[Carg5], regs[Carg6],
                                                                               regs[Carg7]);
          break;
        case pb_call_uptr_double_double_double_double_double_double:
          regs[Cretval] = ((pb_uptr_double_double_double_double_double_double_t)proc)(fpregs[Cfparg1], fpregs[Cfparg2], fpregs[Cfparg3],
                                                                                      fpregs[Cfparg4], fpregs[Cfparg5], fpregs[Cfparg6]);
          break;
        case pb_call_voids:
          regs[Cretval] = TO_PTR(((pb_voids_t)proc)());
          break;
        case pb_call_voids_uptr:
          regs[Cretval] = TO_PTR(((pb_voids_uptr_t)proc)(regs[Carg1]));
          break;
        default:
          S_error_abort("unsupported call prototype");
          break;
        }
      }
      break;
    case COMMON_INSTR(pb_inc_pb_register, 0x133)
    case COMMON_INSTR(pb_inc_pb_immediate, 0x134)
    case COMMON_INSTR(pb_lock, 0x135)
    case COMMON_INSTR(pb_cas, 0x136)
    case COMMON_INSTR(pb_fence_pb_fence_store_store, 0x137)
    case COMMON_INSTR(pb_fence_pb_fence_acquire, 0x138)
    case COMMON_INSTR(pb_fence_pb_fence_release, 0x139)
    case COMMON_INSTR(pb_call_arena_in, 0x13A)
    case COMMON_INSTR(pb_fp_call_arena_in, 0x13B)
    case COMMON_INSTR(pb_call_arena_out, 0x13C)
    case COMMON_INSTR(pb_fp_call_arena_out, 0x13D)
    case COMMON_INSTR(pb_stack_call, 0x13E)
    case pb_chunk:
      next_ip = TO_VOIDP((chunks[INSTR_ii_high(instr)])(ms, TO_PTR(ip), INSTR_ii_low(instr)));
      break;
#ifdef WASM_PBCHUNK
    case 229: // wasm_pb_chunk
     //printf("wasm_pb_chunk\n");
     //printf("instr_high: %d\n ", INSTR_ii_high(instr));
      //printf("current ip is: %p\n", ip);
      //dump_machine_state(ms);
      next_ip = TO_VOIDP(wasm_do_jump(INSTR_ii_high(instr), ms, TO_PTR(ip)));
      //printf("next_ip is: %p\n", next_ip);
      //dump_machine_state(ms);
      break;
#endif
    default:
      S_error_abort("illegal pb instruction");
      break;
    }
    ip = next_ip;
  }
}

ptr *S_get_call_arena(ptr tc) {
  return &PBCALLARENA(tc, 0);
}

#if defined(PTHREADS)
void S_pb_spinlock(void *addr) {
  while (1) {
    if (CAS_ANY_FENCE(addr, TO_VOIDP(0), TO_VOIDP(1)))
      break;
  }
}

int S_pb_locked_adjust(void *addr, int delta) {
  while (1) {
    uptr oldv = *(uptr *)addr;
    uptr newv = oldv + delta;
    if (CAS_ANY_FENCE(addr, TO_VOIDP(oldv), TO_VOIDP(newv)))
      return newv == 0;
  }
}
#endif

#endif
