/*
    Greenfish Image Converter
    Copyright (c) 2016 B. Szalkai

	This software is provided 'as-is', without any express or implied
	warranty. In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

	1. The origin of this software must not be misrepresented; you must not
	   claim that you wrote the original software. If you use this software
	   in a product, an acknowledgement in the product documentation would be
	   appreciated but is not required.
	2. Altered source versions must be plainly marked as such, and must not be
	   misrepresented as being the original software.
	3. This notice may not be removed or altered from any source distribution.
*/
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace ImageConverter
{
	class GfNode
	{
		public byte[] Data;
		public List<GfNode> Children;
		public GfNode Parent;
		public string Id;

		public bool AsBool
		{
			get { return Data.Length >= 1 ? (Data[0] != 0) : false; }
			set { Data = BitConverter.GetBytes(value); }
		}
		public int AsInt
		{
			get { return Data.Length >= sizeof(int) ? BitConverter.ToInt32(Data, 0) : 0; }
			set { Data = BitConverter.GetBytes(value); }
		}
		public long AsLong
		{
			get { return Data.Length >= sizeof(long) ? BitConverter.ToInt64(Data, 0) : 0; }
			set { Data = BitConverter.GetBytes(value); }
		}
		public double AsDouble
		{
			get { return Data.Length >= sizeof(double) ? BitConverter.ToDouble(Data, 0) : 0; }
			set { Data = BitConverter.GetBytes(value); }
		}
		public string AsString
		{
			get { return Encoding.UTF8.GetString(Data); }
			set { Data = Encoding.UTF8.GetBytes(value); }
		}

		public GfNode(GfNode parent)
		{
			Data = new byte[0];
			Children = new List<GfNode>();
			Parent = parent;
			Id = "";
		}

		public void Clear()
		{
			Data = new byte[0];
			Children.Clear();
		}

		public GfNode NewChild()
		{
			GfNode result = new GfNode(this);
			Children.Add(result);
			return result;
		}

		public void Save(BinaryWriter bw)
		{
			bw.Write((byte)GfTree.GFDT_BLOCK_BEGIN);
			byte[] idBytes = Encoding.UTF8.GetBytes(Id);
			bw.Write((byte)idBytes.Length);
			bw.Write(idBytes);
			bw.Write((int)Data.Length);
			bw.Write(Data);
			foreach (var n in Children)
				n.Save(bw);
			bw.Write((byte)GfTree.GFDT_BLOCK_END);
		}
	}

	class GfTree
	{
		public const int GF_DATA_TREE_SIG = 0x74646667; // 'gfdt'
		public const byte GFDT_BLOCK_BEGIN = 60; // <
		public const byte GFDT_BLOCK_END = 62; // >

		public GfNode Root { get; private set; }
		public GfNode CurrentNode { get; private set; }

		public GfTree()
		{
			Root = new GfNode(null);
			Root.Id = "\\";
			CurrentNode = Root;
		}

		public void Clear()
		{
			Root.Clear();
			CurrentNode = Root;
		}

		public GfNode GetChildById(string id)
		{
			return CurrentNode.Children.FirstOrDefault(n => n.Id == id);
		}

		public bool DeleteChildById(string id)
		{
			return CurrentNode.Children.RemoveAll(n => n.Id == id) > 0;
		}

		public bool Descend(string id)
		{
			GfNode n = GetChildById(id);
			if (n != null)
				CurrentNode = n;
			return n != null;
		}

		public void NewChild(string id)
		{
			GfNode n = GetChildById(id);
			if (n != null)
				CurrentNode = n;
			else
			{
				CurrentNode = CurrentNode.NewChild();
				CurrentNode.Id = id;
			}
		}

		public bool Ascend()
		{
			bool ok = (CurrentNode.Parent != null);
			if (ok)
				CurrentNode = CurrentNode.Parent;
			return ok;
		}

		public void AscendToRoot()
		{
			CurrentNode = Root;
		}

		public static bool CanLoad(Stream s)
		{
			if (s.Length - s.Position < 4)
				return false;
			byte[] sig = new byte[4];
			s.Read(sig, 0, sig.Length);
			s.Seek(-sig.Length, SeekOrigin.Current);
			return BitConverter.ToInt32(sig, 0) == GfTree.GF_DATA_TREE_SIG;
		}

		public bool Load(BinaryReader br)
		{
			try
			{
				int sig = br.ReadInt32();
				if (sig != GF_DATA_TREE_SIG)
					return false;

				Clear();
				bool RootWasRead = false;

				// read blocks
				while (br.BaseStream.Position < br.BaseStream.Length)
				{
					byte BlockType = br.ReadByte();
					bool endReached = false;

					switch (BlockType)
					{
						case GFDT_BLOCK_BEGIN:
							// read ID
							int idLength = br.ReadByte();
							string id = Encoding.UTF8.GetString(br.ReadBytes(idLength));

							if (RootWasRead)
								NewChild(id);
							else
							{
								CurrentNode.Id = id;
								RootWasRead = true;
							}

							// read data size
							int DataSize = br.ReadInt32();
							CurrentNode.Data = br.ReadBytes(DataSize);
							break;

						case GFDT_BLOCK_END:
							if (!Ascend())
								endReached = true;
							break;

						default:
							return false;
					}

					if (endReached)
						break;
				}

				if (CurrentNode != Root)
					return false;

				return true;
			}
			catch
			{
				return false;
			}
		}

		public void Save(BinaryWriter bw)
		{
			bw.Write((int)GF_DATA_TREE_SIG);
			Root.Save(bw);
		}

		public bool Load(Stream s)
		{
			using (var br = new BinaryReader(s))
				return Load(br);
		}

		public void Save(Stream s)
		{
			using (var bw = new BinaryWriter(s))
				Save(bw);
		}

		public bool Load(string filename)
		{
			using (var fs = new FileStream(filename, FileMode.Open, FileAccess.Read))
				return Load(fs);
		}

		public void Save(string filename)
		{
			using (var fs = new FileStream(filename, FileMode.Create, FileAccess.Write))
				Save(fs);
		}
	}
}
