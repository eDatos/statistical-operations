package org.siemac.metamac.statistical.operations.rest.internal;

import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Link;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;

public class RestMocks {

    public static InternationalString mockInternationalString(String locale1, String label1, String locale2, String label2) {

        InternationalString internationalString = new InternationalString();

        LocalisedString internationalStringLocale1 = new LocalisedString();
        internationalStringLocale1.setLocale(locale1);
        internationalStringLocale1.setLabel(label1);
        internationalString.getTexts().add(internationalStringLocale1);

        LocalisedString internationalStringLocale2 = new LocalisedString();
        internationalStringLocale2.setLocale(locale2);
        internationalStringLocale2.setLabel(label2);
        internationalString.getTexts().add(internationalStringLocale2);

        return internationalString;
    }

    public static Resource mockResource(String id, String kind, String rel, String href, Boolean addTitle, String subtitle) {
        Resource resource = new Resource();
        resource.setId(id);
        resource.setKind(kind);
        resource.setLink(mockLink(rel, href));
        if (addTitle) {
            resource.setTitle(mockInternationalString("es", "Título " + subtitle + " " + id, "en", "Title " + subtitle + " " + id));
        }
        return resource;
    }

    public static Link mockLink(String rel, String href) {
        Link link = new Link();
        link.setRel(rel);
        link.setHref(href);
        return link;
    }
}
