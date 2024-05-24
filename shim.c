#include <stdint.h>
#include <stdlib.h>
#include <tree_sitter/api.h>

/******************/
/* Section - Tree */
/******************/

TSNode *ts_tree_root_node_(const TSTree *self)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_tree_root_node(self);
	return node;
}

TSNode *ts_tree_root_node_with_offset_(const TSTree *self, uint32_t offset_bytes, TSPoint offset_extent)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_tree_root_node_with_offset(self, offset_bytes, offset_extent);
	return node;
}

/******************/
/* Section - Node */
/******************/

const char *ts_node_type_(TSNode *self)
{
	return ts_node_type(*self);
}

TSSymbol ts_node_symbol_(TSNode *self)
{
	return ts_node_symbol(*self);
}

const TSLanguage *ts_node_language_(TSNode *self)
{
	return ts_node_language(*self);
}

const char *ts_node_grammar_type_(TSNode *self)
{
	return ts_node_grammar_type(*self);
}

TSSymbol ts_node_grammar_symbol_(TSNode *self)
{
	return ts_node_grammar_symbol(*self);
}

uint32_t ts_node_start_byte_(TSNode *self)
{
	return ts_node_start_byte(*self);
}

TSPoint ts_node_start_point_(TSNode *self)
{
	return ts_node_start_point(*self);
}

uint32_t ts_node_end_byte_(TSNode *self)
{
	return ts_node_end_byte(*self);
}

TSPoint ts_node_end_point_(TSNode *self)
{
	return ts_node_end_point(*self);
}

char *ts_node_string_(TSNode *self)
{
	return ts_node_string(*self);
}

bool ts_node_is_null_(TSNode *self)
{
	return ts_node_is_null(*self);
}

bool ts_node_is_named_(TSNode *self)
{
	return ts_node_is_named(*self);
}

bool ts_node_is_missing_(TSNode *self)
{
	return ts_node_is_missing(*self);
}

bool ts_node_is_extra_(TSNode *self)
{
	return ts_node_is_extra(*self);
}

bool ts_node_has_changes_(TSNode *self)
{
	return ts_node_has_changes(*self);
}

bool ts_node_has_error_(TSNode *self)
{
	return ts_node_has_error(*self);
}

bool ts_node_is_error_(TSNode *self)
{
	return ts_node_is_error(*self);
}

TSStateId ts_node_parse_state_(TSNode *self)
{
	return ts_node_parse_state(*self);
}

TSStateId ts_node_next_parse_state_(TSNode *self)
{
	return ts_node_next_parse_state(*self);
}

TSNode *ts_node_parent_(TSNode *self)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_parent(*self);
	return node;
}

TSNode *ts_node_child_containing_descendant_(TSNode *self, TSNode *descendant)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_child_containing_descendant(*self, *descendant);
	return node;
}

TSNode *ts_node_child_(TSNode *self, uint32_t child_index)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_child(*self, child_index);
	return node;
}

const char *ts_node_field_name_for_child_(TSNode *self, uint32_t child_index)
{
	return ts_node_field_name_for_child(*self, child_index);
}

uint32_t ts_node_child_count_(TSNode *self)
{
	return ts_node_child_count(*self);
}

TSNode *ts_node_named_child_(TSNode *self, uint32_t child_index)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_named_child(*self, child_index);
	return node;
}

uint32_t ts_node_named_child_count_(TSNode *self)
{
	return ts_node_named_child_count(*self);
}

TSNode *ts_node_child_by_field_id_(TSNode *self, TSFieldId field_id)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_child_by_field_id(*self, field_id);
	return node;
}

TSNode *ts_node_next_sibling_(TSNode *self)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_next_sibling(*self);
	return node;
}

TSNode *ts_node_prev_sibling_(TSNode *self)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_prev_sibling(*self);
	return node;
}

TSNode *ts_node_next_named_sibling_(TSNode *self)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_next_named_sibling(*self);
	return node;
}

TSNode *ts_node_prev_named_sibling_(TSNode *self)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_prev_named_sibling(*self);
	return node;
}

TSNode *ts_node_first_child_for_byte_(TSNode *self, uint32_t byte)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_first_child_for_byte(*self, byte);
	return node;
}

TSNode *ts_node_first_named_child_for_byte_(TSNode *self, uint32_t byte)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_first_named_child_for_byte(*self, byte);
	return node;
}

uint32_t ts_node_descendant_count_(TSNode *self)
{
	return ts_node_descendant_count(*self);
}

TSNode *ts_node_descendant_for_byte_range_(TSNode *self, uint32_t start, uint32_t end)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_descendant_for_byte_range(*self, start, end);
	return node;
}

TSNode *ts_node_descendant_for_point_range_(TSNode *self, TSPoint start, TSPoint end)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_descendant_for_point_range(*self, start, end);
	return node;
}

TSNode *ts_node_named_descendant_for_byte_range_(TSNode *self, uint32_t start, uint32_t end)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_named_descendant_for_byte_range(*self, start, end);
	return node;
}

TSNode *ts_node_named_descendant_for_point_range_(TSNode *self, TSPoint start, TSPoint end)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_node_named_descendant_for_point_range(*self, start, end);
	return node;
}

bool ts_node_eq_(TSNode *self, TSNode *other)
{
	return ts_node_eq(*self, *other);
}

void ts_node_delete(TSNode *self)
{
	free(self);
}

/************************/
/* Section - TreeCursor */
/************************/

TSTreeCursor *ts_tree_cursor_new_(TSNode *node)
{
	TSTreeCursor *cursor = malloc(sizeof(TSTreeCursor));
	if (cursor) *cursor = ts_tree_cursor_new(*node);
	return cursor;
}

void ts_tree_cursor_reset_(TSTreeCursor *self, TSNode *node)
{
	ts_tree_cursor_reset(self, *node);
}

TSNode *ts_tree_cursor_current_node_(const TSTreeCursor *self)
{
	TSNode *node = malloc(sizeof(TSNode));
	if (node) *node = ts_tree_cursor_current_node(self);
	return node;
}
