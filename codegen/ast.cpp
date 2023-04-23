#include <cstdlib>
#include <cstdio>

#include "ast.hpp"

AstNode::AstNode(AstType type)
{
	this->type = type;
	this->value.s = nullptr;
	this->parent = nullptr;
	this->lineNumber = 0;
}

AstNode::AstNode(AstType type, const char* value)
{
	this->type = type;
	this->value.s = value;
	this->parent = nullptr;
	this->lineNumber = 0;
}

UnaryNode::UnaryNode(AstType type, AstNode* child) :
	AstNode(type)
{
	this->child = child;
}

BinaryNode::BinaryNode(AstType type, AstNode* lhs, AstNode* rhs) :
	AstNode(type)
{
	this->lhs = lhs;
	this->rhs = rhs;
}

RootNode::RootNode() :
	AstNode(AST_ROOT)
{
}

ListNode::ListNode() :
	AstNode(AST_LIST)
{
	this->next = nullptr;
}

DeclNode::DeclNode(const char* name, AstNode* expression) :
	AstNode(AST_DECL)
{
	this->value.s = name;
	this->expression = expression;
}

LabelNode::LabelNode(const char* name, AstNode* child) :
	AstNode(AST_LABEL)
{
	this->labelType = LABEL_NONE;
	this->value.s = name;
	this->child = child;
}

InstructionNode::InstructionNode(int code) :
	AstNode(AST_INSTRUCTION)
{
	this->code = code;
}

InstructionNode::InstructionNode(int code, const void* operand) :
	AstNode(AST_INSTRUCTION)
{
	this->code = code;
	this->value.ptr = operand;
}

static ListNode* reverseList(ListNode* prev, ListNode* node)
{
	// Swap next links until we hit the tail, then return it
	//
	ListNode* tail = nullptr;
	if (node->next == nullptr)
	{
		// We're done
		//
		node->next = prev;
		tail = node;
	}
	else
	{
		ListNode* next = node->next;

		// Switch directions of the pointers on this node and the next one
		//
		node->next = prev;
		tail = reverseList(node, next);
	}

	return tail;
}

static void cleanupNode(AstNode** node);

static void cleanupList(ListNode* node)
{
	if (node != nullptr)
	{
		cleanupNode(&node->value.node);
		cleanupList(node->next);
	}
}

void cleanupNode(AstNode** node)
{
	if (*node != nullptr)
	{
		switch ((*node)->type)
		{
		case AST_LIST:
		{
			auto* n = static_cast<ListNode*>(*node);
			*node = reverseList(nullptr, n);

			cleanupList(static_cast<ListNode*>(*node));
		}
		break;
		case AST_DATA8:
		{
			cleanupNode(&(*node)->value.node);
		}
		break;
		case AST_DATA16:
		{
			cleanupNode(&(*node)->value.node);
		}
		break;
		case AST_DECL:
		{
			auto* d = static_cast<DeclNode*>(*node);
			cleanupNode(&d->expression);
		}
		break;
		case AST_LABEL:
		{
			auto* n = static_cast<LabelNode*>(*node);
			cleanupNode(&n->child);
		}
		break;
		default:
			break;
		}
	}
}

void cleanupAst(RootNode* root)
{
	for (auto it = root->children.begin(); it != root->children.end(); ++it)
	{
		AstNode* node = *it;
		cleanupNode(&node);
	}
}

static void printIndent(int indent)
{
	for (int i = 0; i < indent; i++)
	{
		printf(" ");
	}
}

void printNode(AstNode* node, int indent)
{
	if (node != nullptr)
	{
		switch (node->type)
		{
		case AST_LIST:
		{
			auto* n = static_cast<ListNode*>(node);
			printIndent(indent);
			printf("list element:\n");
			printNode(n->value.node, indent + 4);
			printNode(n->next, indent);
		}
		break;
		case AST_DATA8:
		{
			printIndent(indent);
			printf("data:\n");
			printNode(node->value.node, indent + 4);
		}
		break;
		case AST_DATA16:
		{
			printIndent(indent);
			printf("data (16 bit):\n");
			printNode(node->value.node, indent + 4);
		}
		break;
		case AST_DECL:
		{
			auto* d = static_cast<DeclNode*>(node);
			printIndent(indent);
			printf("decl: %s = \n", d->value.s);
			printNode(d->expression, indent + 4);
		}
		break;
		case AST_LABEL:
		{
			auto* n = static_cast<LabelNode*>(node);
			printIndent(indent);
			printf("label: %s\n", n->value.s);
			printNode(n->child, indent + 4);
		}
		break;
		case AST_NAME:
			printIndent(indent);
			printf("name: %s\n", node->value.s);
			break;
		case AST_CONST:
			printIndent(indent);
			printf("constant: %s\n", node->value.s);
			break;
		case AST_IMMEDIATE:
		{
			auto* n = static_cast<UnaryNode*>(node);
			printIndent(indent);
			printf("immediate:\n");
			printNode(n->child, indent + 4);
		}
		break;
		case AST_INDIRECT:
		{
			auto* n = static_cast<UnaryNode*>(node);
			printIndent(indent);
			printf("indirect:\n");
			printNode(n->child, indent + 4);
		}
		break;
		case AST_INDEXED_X:
		{
			auto* n = static_cast<UnaryNode*>(node);
			printIndent(indent);
			printf("indexed x:\n");
			printNode(n->child, indent + 4);
		}
		break;
		case AST_INDEXED_Y:
		{
			auto* n = static_cast<UnaryNode*>(node);
			printIndent(indent);
			printf("indexed y:\n");
			printNode(n->child, indent + 4);
		}
		break;
		case AST_LOBYTE:
		{
			auto* n = static_cast<UnaryNode*>(node);
			printIndent(indent);
			printf("low byte:\n");
			printNode(n->child, indent + 4);
		}
		break;
		case AST_HIBYTE:
		{
			auto* n = static_cast<UnaryNode*>(node);
			printIndent(indent);
			printf("high byte:\n");
			printNode(n->child, indent + 4);
		}
		break;
		case AST_ADD:
		{
			auto* n = static_cast<BinaryNode*>(node);
			printIndent(indent);
			printf("add:\n");
			printNode(n->lhs, indent + 4);
			printNode(n->rhs, indent + 4);
		}
		break;
		case AST_SUBTRACT:
		{
			auto* n = static_cast<BinaryNode*>(node);
			printIndent(indent);
			printf("subtract:\n");
			printNode(n->lhs, indent + 4);
			printNode(n->rhs, indent + 4);
		}
		break;
		case AST_INSTRUCTION:
		{
			auto* n = static_cast<InstructionNode*>(node);
			printIndent(indent);
			printf("instruction (line %d) %d:\n", n->lineNumber, n->code);
			printNode(n->value.node, indent + 4);
		}
		break;
		default:
			break;
		}
	}
}