#include "../x.h"
#include "vram.h"

static int vram_size[VRAM_COUNT] = {
    [VRAM_0] = VRAM_0_U32_SIZE,
    [VRAM_1] = VRAM_1_U32_SIZE,
    [VRAM_A] = VRAM_A_U32_SIZE,
    [VRAM_B] = VRAM_B_U32_SIZE,
    [VRAM_C] = VRAM_C_U32_SIZE,
    [VRAM_D] = VRAM_D_U32_SIZE,
    [VRAM_E] = VRAM_E_U32_SIZE,
    [VRAM_F] = VRAM_F_U32_SIZE,
    [VRAM_G] = VRAM_G_U32_SIZE,
    [VRAM_H] = VRAM_H_U32_SIZE,
    [VRAM_I] = VRAM_I_U32_SIZE,
    [VRAM_AB] = VRAM_AB_U32_SIZE,
    [VRAM_CD] = VRAM_CD_U32_SIZE,
    [VRAM_ABCD] = VRAM_ABCD_U32_SIZE
};

static uint32_t *vram_ptr[VRAM_COUNT] = {
    [VRAM_0] = VRAM_0_U32_PTR,
    [VRAM_1] = VRAM_1_U32_PTR,
    [VRAM_A] = VRAM_A_U32_PTR,
    [VRAM_B] = VRAM_B_U32_PTR,
    [VRAM_C] = VRAM_C_U32_PTR,
    [VRAM_D] = VRAM_D_U32_PTR,
    [VRAM_E] = VRAM_E_U32_PTR,
    [VRAM_F] = VRAM_F_U32_PTR,
    [VRAM_G] = VRAM_G_U32_PTR,
    [VRAM_H] = VRAM_H_U32_PTR,
    [VRAM_I] = VRAM_I_U32_PTR,
    [VRAM_AB] = VRAM_AB_U32_PTR,
    [VRAM_CD] = VRAM_D_U32_PTR,
    [VRAM_ABCD] = VRAM_ABCD_U32_PTR,
};

int
vram_get_u32_size (vram_bank_t bank)
{
    return vram_size[bank];
}

uint32_t *
vram_get_u32_ptr (vram_bank_t bank)
{
    return vram_ptr[bank];
}

void
vram_zero_bank (vram_bank_t bank)
{
    memset (vram_ptr[bank], 0, vram_size[bank] * sizeof(uint32_t));
}

SCM_VARIABLE_INIT (G_VRAM_0, "VRAM_0", scm_from_int (VRAM_0));
SCM_VARIABLE_INIT (G_VRAM_1, "VRAM_1", scm_from_int (VRAM_1));
SCM_VARIABLE_INIT (G_VRAM_A, "VRAM_A", scm_from_int (VRAM_A));
SCM_VARIABLE_INIT (G_VRAM_B, "VRAM_B", scm_from_int (VRAM_B));
SCM_VARIABLE_INIT (G_VRAM_C, "VRAM_C", scm_from_int (VRAM_C));
SCM_VARIABLE_INIT (G_VRAM_D, "VRAM_D", scm_from_int (VRAM_D));
SCM_VARIABLE_INIT (G_VRAM_E, "VRAM_E", scm_from_int (VRAM_E));
SCM_VARIABLE_INIT (G_VRAM_F, "VRAM_F", scm_from_int (VRAM_F));
SCM_VARIABLE_INIT (G_VRAM_G, "VRAM_G", scm_from_int (VRAM_G));
SCM_VARIABLE_INIT (G_VRAM_H, "VRAM_H", scm_from_int (VRAM_H));
SCM_VARIABLE_INIT (G_VRAM_I, "VRAM_I", scm_from_int (VRAM_I));
SCM_VARIABLE_INIT (G_VRAM_AB, "VRAM_AB", scm_from_int (VRAM_AB));
SCM_VARIABLE_INIT (G_VRAM_CD, "VRAM_CD", scm_from_int (VRAM_CD));
SCM_VARIABLE_INIT (G_VRAM_ABCD, "VRAM_ABCD", scm_from_int (VRAM_ABCD));

void
vram_init_guile_procedures (void)
{
#include "vram.x"
  scm_c_export ("VRAM_0",
                "VRAM_1",
                "VRAM_A",
                "VRAM_B",
                "VRAM_C",
                "VRAM_D",
                "VRAM_E",
                "VRAM_F",
                "VRAM_G",
                "VRAM_H",
                "VRAM_I",
                "VRAM_AB",
                "VRAM_CD",
                "VRAM_ABCD",
                NULL);
}


/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/

