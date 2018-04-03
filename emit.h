#ifndef EMIT_H
#define EMIT_H

#include "parse.h"

uint32_t emit_code(CompilerCtx *cctx, AstNode *node);
void emit_code_block(CompilerCtx *cctx, AstNode *node);
void strip_labels(CompilerCtx *cctx);

#endif
