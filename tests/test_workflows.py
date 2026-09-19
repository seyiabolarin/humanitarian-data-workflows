import csv
from pathlib import Path
import sys
import tempfile
import unittest
from PIL import Image
from pypdf import PdfReader

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "python"))
from workflows import load_register, clean_image, compress_folder, placeholder, register_pdf, prepare_local_attachments, local_picture


class WorkflowTests(unittest.TestCase):
    def test_duplicate_ids_are_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "rows.csv"
            p.write_text("record_id,name,community,lga,household_size\nX,A,C,L,2\nX,B,C,L,3\n")
            with self.assertRaises(ValueError):
                load_register(p)

    def test_source_unchanged_metadata_removed_and_no_collision(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            source = base / "input"
            source.mkdir()
            img = Image.new("RGB", (200, 100), "red")
            exif = Image.Exif()
            exif[274] = 6
            exif[315] = "Fictional test author"
            img.save(source / "x.jpg", exif=exif)
            img.save(source / "x.png")
            before = (source / "x.jpg").read_bytes()
            outputs = compress_folder(source, base / "output")
            self.assertEqual(len(outputs), 2)
            self.assertEqual((source / "x.jpg").read_bytes(), before)
            with Image.open(base / "output/x.jpg.jpg") as result:
                self.assertEqual(result.size, (100, 200))
                self.assertFalse(result.getexif())
            with self.assertRaises(ValueError):
                clean_image(source / "x.jpg", source / "x.jpg")
            with self.assertRaises(ValueError):
                compress_folder(source, source / "child")

    def test_pdf_contains_card_and_amount_and_paginates(self):
        rows = load_register()
        rows[0]["name"] = "Example <A> & B"
        rows = [dict(rows[i % 3], record_id=f"DEMO-{i:03}", lga="Example District") for i in range(120)]
        with tempfile.TemporaryDirectory() as tmp:
            path = register_pdf(rows, Path(tmp) / "demo.pdf", variant="cash")
            reader = PdfReader(path)
            self.assertGreater(len(reader.pages), 1)
            content = "\n".join(p.extract_text() for p in reader.pages)
            self.assertIn("DEMO-CARD-001", content)
            self.assertIn("Amount (demo units)", content)
            self.assertIn("Example <A> & B", content)
            self.assertIn("DEMO-119", content)
            for page in reader.pages:
                self.assertIn("Record ID", page.extract_text())

    def test_local_attachment_manifest_has_no_source_names(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            p = placeholder(root / "input/name-not-for-publication.png")
            manifest = prepare_local_attachments(p.parent, root / "output")
            self.assertEqual(len(manifest), 1)
            self.assertNotIn("name-not-for-publication", str(manifest))
            self.assertTrue((root / "output" / manifest[0]["output"]).is_file())

    def test_picture_path_cannot_escape_and_source_is_preserved(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            p = placeholder(root / "images/sample.png")
            before = p.read_bytes()
            rows = [dict(r, photo_filename="sample.png") for r in load_register()]
            result = register_pdf(rows, root / "pictures.pdf", image_root=p.parent)
            self.assertTrue(result.is_file())
            self.assertEqual(p.read_bytes(), before)
            with self.assertRaises(ValueError):
                local_picture(p.parent, "../pictures.pdf")


if __name__ == "__main__":
    unittest.main()
